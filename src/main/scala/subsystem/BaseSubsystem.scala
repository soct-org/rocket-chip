// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.diplomacy.AddressRange
import freechips.rocketchip.resources.{AddressMapEntry, BindingScope, DTB, DTS, DTSCompat, DTSModel, DTSTimebase, JSON, Resource, ResourceAnchors, ResourceBinding, ResourceInt, ResourceString}
import freechips.rocketchip.prci.{ClockBundle, ClockGroupAggregator, ClockGroupIdentityNode, ClockGroupSourceNode, ClockGroupSourceParameters, ClockSinkDomain}
import freechips.rocketchip.tilelink.TLBusWrapper
import freechips.rocketchip.util.{ElaborationArtefacts, Location, PlusArgArtefacts, RecordMap}

case object SubsystemDriveClockGroupsFromIO extends Field[Boolean](true)

/**
 * When set to `true`, prints a deterministic mapping from each
 * `io_clocks` field (e.g. `aggregator_0`) to the TL bus clock domains
 * it ultimately drives (e.g. `SBUS:sbus_0`, `MBUS:mbus_0`).
 *
 * This flag only has an effect when [[SubsystemDriveClockGroupsFromIO]]
 * is also enabled.  Printing is intentionally disabled by default to
 * avoid noise in normal elaborations.
 */
case object PrintClockGroupMapping extends Field[Boolean](false)

case class TLNetworkTopologyLocated(where: HierarchicalLocation) extends Field[Seq[CanInstantiateWithinContextThatHasTileLinkLocations with CanConnectWithinContextThatHasTileLinkLocations]]

case class TLManagerViewpointLocated(where: HierarchicalLocation) extends Field[Location[TLBusWrapper]](SBUS)

class HierarchicalLocation(override val name: String) extends Location[LazyScope](name)

case object InTile extends HierarchicalLocation("InTile")

case object InSubsystem extends HierarchicalLocation("InSubsystem")

case object InSystem extends HierarchicalLocation("InSystem")

// HasDts is generating metadatas from Scala, which is not the target for new diplomacy and Property.
// It will be deprecated and removed after we migrate all metadata handling logic to OM Dialect.
trait HasDTS extends LazyModule with BindingScope {
  lazy val dts = DTS(bindingTree)
  lazy val dtb = DTB(dts)
  lazy val json = JSON(bindingTree)
}

trait HasDTSImp[+L <: HasDTS] {
  this: LazyRawModuleImp =>
  def dtsLM: L
  // GraphML should live outside form this trait, but we keep it here until we find an appropriate way to handle metadata
  ElaborationArtefacts.add("graphml", dtsLM.graphML)
  // PlusArg should be purged out from rocket-chip in a near feature.
  ElaborationArtefacts.add("plusArgs", PlusArgArtefacts.serialize_cHeader())
  ElaborationArtefacts.add("dts", dtsLM.dts)
  ElaborationArtefacts.add("json", dtsLM.json)
  println(dtsLM.dts)
}

trait SubsystemResetScheme

/** Reset scheme for the io_clocks ports only */
case object ResetSynchronous extends SubsystemResetScheme

/** Reset scheme for the io_clocks ports and several other top-level resets */
case object ResetSynchronousFull extends SubsystemResetScheme

/** Reset scheme for the io_clocks ports only */
case object ResetAsynchronous extends SubsystemResetScheme

/** Reset scheme for the io_clocks ports and several other top-level resets */
case object ResetAsynchronousFull extends SubsystemResetScheme

/** Reset scheme for the subsystem - io_clocks and other resets */
case object SubsystemResetSchemeKey extends Field[SubsystemResetScheme](ResetSynchronousFull)

/** Determine the top-level reset type based on the SubsystemResetSchemeKey */
object DetermineTopLevelResetType {
  def apply()(implicit p: Parameters): Reset = {
    p(SubsystemResetSchemeKey) match {
      case ResetSynchronousFull => Bool()
      case ResetAsynchronousFull => AsyncReset()
      case _ => Reset() // Default to abstract Reset
    }
  }
}

/**
 * Some platforms (FPGA top-levels, simulation harnesses, or
 * externally-managed SoCs) require the subsystem's clock groups
 * to be driven from top-level IO rather than generated internally.
 *
 * When SubsystemDriveClockGroupsFromIO is enabled, we expose each
 * clock group as an IO port so that the environment (board, testbench,
 * or chip top) owns clock generation and reset sequencing.
 *
 * When disabled, clock groups are expected to be sourced internally
 * by PRCI nodes (PLLs, dividers, clock muxes, etc.).
 *
 * == Clock-group IO port naming ==
 *
 * Each exposed port is named after the diplomatic member key of the
 * corresponding [[ClockGroupSourceNode]] output edge, e.g.
 * `aggregator_0`, `aggregator_1`, ...  These names appear in the
 * generated RTL as `io_clocks_aggregator_0_clock`, etc.
 *
 * == Mapping to TL bus clock domains ==
 *
 * The mapping from IO port names to the [[TLBusWrapper]] clock domains
 * they ultimately drive is topology-dependent (it varies with
 * [[DriveClocksFromSBus]], crossing types, etc.).  You can print this
 * mapping at elaboration time by enabling [[PrintClockGroupMapping]].
 *
 * The mapping is derived structurally: each `ClockSinkParameters`
 * object is used as a unique token.  The same object that a device or
 * bus registered as its clock requirement propagates—unchanged—through
 * the aggregation chain all the way to the `ClockGroupSourceNode`, so
 * reference equality (`eq`) reliably links an IO port to the bus
 * domains it serves.
 */
trait HasConfigurablePRCILocations {
  this: HasTileLinkLocations =>

  // Interrupt bus is always present; unrelated to clocking policy
  lazy val ibus: InterruptBusWrapper = LazyModule(new InterruptBusWrapper)

  // Identity node that represents the full set of clock groups
  // used by the subsystem. Policy determines how these clocks
  // are sourced; mechanisms below implement that policy.
  lazy val allClockGroupsNode = ClockGroupIdentityNode()


  val io_clocks: Option[ModuleValue[RecordMap[ClockBundle]]] =
    if (p(SubsystemDriveClockGroupsFromIO)) {
      Some(buildClockGroupIO())
    } else {
      None
    }

  /**
   * This method implements the policy of "drive clock groups from IO".
   * It does so by:
   *  1. Creating a ClockGroupSourceNode that represents externally-supplied clocks.
   *  2. Aggregating those clocks so they participate in the unified clock group graph.
   *  3. Materializing IO ports that directly drive the internal ClockBundle instances.
   *  4. Optionally printing the mapping from IO port names to TL bus clock domains
   *     when [[PrintClockGroupMapping]] is enabled.
   *
   * No clocking decisions are made here; this is purely a structural
   * realization of the policy selected above.
   *
   * == IO port → bus domain mapping ==
   *
   * Each IO port field (e.g. `aggregator_3`) ultimately drives one or
   * more TL-bus clock domains.  The mapping is printed at elaboration
   * time when [[PrintClockGroupMapping]] is `true`, and shows entries
   * such as:
   * {{{
   *   io_clocks.aggregator_3  ->  SBUS:sbus_3  MBUS:mbus_0
   * }}}
   *
   * When `DriveClocksFromSBus = true`, buses like MBUS and COH are
   * clocked transitively through SBUS; they still appear in the
   * mapping (under their own bus location) because their
   * `ClockSinkParameters` objects flow all the way up through the
   * aggregation graph to the source node.
   */
  private def buildClockGroupIO(): ModuleValue[RecordMap[ClockBundle]] = {
    val aggregator = ClockGroupAggregator()
    val source = ClockGroupSourceNode(Seq(ClockGroupSourceParameters()))

    // Connect externally-driven clock groups into the global clock
    // group graph so downstream consumers see no distinction between
    // internal and IO-sourced clocks.
    allClockGroupsNode :*= aggregator := source

    InModuleBody {
      val elements = source.out.flatMap { case (bundle, _) =>
        bundle.member.elements
      }
      val io = clockGroupIO(elements)

      // Mechanically wire each IO clock bundle to its corresponding
      // internal clock group member.
      elements.foreach { case (name, data) =>
        io(name).foreach {
          data := _
        }
      }

      // Optionally print the IO-port → TL-bus-domain mapping at
      // elaboration time.  This is guarded by a config key so that
      // normal elaborations are not made noisier.
      if (p(PrintClockGroupMapping)) {
        printClockGroupMapping(source)
      }

      io
    }
  }

  /**
   * Prints a deterministic mapping from each `io_clocks` field to the
   * [[TLBusWrapper]] clock domains it ultimately drives.
   *
   * === Algorithm ===
   *
   * The diplomatic framework preserves `ClockSinkParameters` object
   * identity (reference equality, `eq`) through the entire aggregation
   * chain from individual clock sinks up to the top-level
   * [[ClockGroupSourceNode]].  We exploit this to link IO port indices
   * (from `source.out`) to bus member names (from each bus's
   * `clockGroupNode.in` edges) without making any assumptions about
   * member-key naming or ordering across different nodes.
   *
   * @param source the [[ClockGroupSourceNode]] created in
   *               [[buildClockGroupIO]]
   */
  private def printClockGroupMapping(source: ClockGroupSourceNode): Unit = {
    require(
      source.out.size == 1,
      "ClockGroupSourceNode in buildClockGroupIO should have exactly one output port"
    )

    val sourceEdge = source.out(0)._2

    // IO port member keys, indexed in the same order as sourceEdge.sink.members.
    // These become the field names in the io_clocks RecordMap (e.g. "aggregator_0").
    val ioPortNames      = sourceEdge.members.keys.toIndexedSeq
    // The ClockSinkParameters object for each IO port, by position.
    // These are the SAME Scala objects that each downstream clock sink
    // (device / bus) registered — reference equality is safe for matching.
    val ioClockSinkParams = sourceEdge.sink.members.toIndexedSeq
    require(ioPortNames.size == ioClockSinkParams.size)

    // Standard TL bus locations to check for clock domain membership
    val busLocs = Seq[TLBusWrapperLocation](SBUS, COH, MBUS, CBUS, PBUS, FBUS)

    // Collect all (ioPortIdx, busLocationName, busMemberKey) triples by
    // matching ClockSinkParameters object identity across the graph.
    // Reference equality is correct here: the aggregation chain preserves
    // the exact Scala objects that each downstream sink registered.
    val allMatches: Seq[(Int, String, String)] =
      for {
        loc               <- busLocs
        bus               <- tlBusWrapperLocationMap.get(loc).toSeq
        // bus.clockGroupNode.in gives inward edges of the bus aggregator
        // (edges from upstream clock providers: allClockGroupsNode or a
        // parent bus when DriveClocksFromSBus is enabled).
        (_, busEdge)      <- bus.clockGroupNode.in
        busMemberNames     = busEdge.members.keys.toIndexedSeq
        (busSink, busIdx) <- busEdge.sink.members.zipWithIndex
        (ioSink,  ioIdx)  <- ioClockSinkParams.zipWithIndex
        if ioSink eq busSink
      } yield (ioIdx, loc.name, busMemberNames(busIdx))

    // Group matches by IO port index, preserving bus-location order
    val mappingByIdx: Map[Int, Seq[(String, String)]] =
      allMatches
        .groupBy(_._1)
        .map { case (k, vs) => k -> vs.map { case (_, b, m) => (b, m) } }

    println("=== Clock Group IO Mapping (io_clocks \u2192 TL bus clock domains) ===")
    ioPortNames.zipWithIndex.foreach { case (ioName, i) =>
      mappingByIdx.get(i) match {
        case Some(targets) =>
          val chain = targets.map { case (bus, member) => s"$bus:$member" }.mkString("  ")
          println(s"  io_clocks.$ioName  ->  $chain")
        case None =>
          println(s"  io_clocks.$ioName  ->  (no TL bus clock domain found)")
      }
    }
    println("=== End Clock Group IO Mapping ===")

    // Sanity check: warn if any IO port has no bus mapping while buses
    // are known to be present.  This can legitimately happen when a
    // port drives only non-TLBus consumers (tiles, debug modules, etc.)
    // but is worth surfacing so users can verify intent.
    if (busLocs.exists(loc => tlBusWrapperLocationMap.get(loc).isDefined)) {
      val unmapped = ioPortNames.indices
        .collect { case i if !mappingByIdx.contains(i) => ioPortNames(i) }
      if (unmapped.nonEmpty) {
        println(
          s"[ClockMapping WARNING] The following io_clocks ports have no " +
          s"direct mapping to any of the standard TL bus locations " +
          s"(${busLocs.map(_.name).mkString(",")}): ${unmapped.mkString(", ")}. " +
          "They may drive non-TLBus consumers (tiles, interrupts, debug, etc.)."
        )
      }
    }
  }

  /**
   * Construct IO ports for each clock group.
   *
   * The reset type of each clock bundle is determined by the
   * subsystem-wide reset scheme. This ensures that externally-driven
   * clocks obey the same reset semantics as internally-generated ones.
   */
  private def clockGroupIO(elements: Seq[(String, ClockBundle)]): RecordMap[ClockBundle] = {

    val resetType = p(SubsystemResetSchemeKey) match {
      case ResetSynchronous | ResetSynchronousFull =>
        () => Bool()
      case ResetAsynchronous | ResetAsynchronousFull =>
        () => AsyncReset()
    }

    IO(Flipped(RecordMap(elements.map { case (name, data) =>
      name -> new ClockBundle(
        params = data.params,
        resetType = Some(resetType)
      )
    }: _*)))
  }
}


/** Look up the topology configuration for the TL buses located within this layer of the hierarchy */
trait HasConfigurableTLNetworkTopology {
  this: HasTileLinkLocations =>
  val location: HierarchicalLocation

  // Calling these functions populates tlBusWrapperLocationMap and connects the locations to each other.
  val topology = p(TLNetworkTopologyLocated(location))
  topology.foreach(_.instantiate(this))
  topology.foreach(_.connect(this))

  def viewpointBus: TLBusWrapper = tlBusWrapperLocationMap(p(TLManagerViewpointLocated(location)))

  // This is used lazily at DTS binding time to get a view of the network
  lazy val topManagers = viewpointBus.unifyManagers
}