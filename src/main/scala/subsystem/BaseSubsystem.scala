// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.diplomacy.AddressRange
import freechips.rocketchip.resources.{AddressMapEntry, BindingScope, DTB, DTS, DTSCompat, DTSModel, DTSTimebase, JSON, Resource, ResourceAnchors, ResourceBinding, ResourceInt, ResourceString}
import freechips.rocketchip.prci.{ClockBundle, ClockEdgeParameters, ClockGroupAggregator, ClockGroupEdgeParameters, ClockGroupIdentityNode, ClockGroupSourceNode, ClockGroupSourceParameters, ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.tilelink.TLBusWrapper
import freechips.rocketchip.util.{ElaborationArtefacts, Location, PlusArgArtefacts, RecordMap}

import scala.collection.mutable

case object SubsystemDriveClockGroupsFromIO extends Field[Boolean](true)

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
 */
trait HasConfigurablePRCILocations {
  this: HasTileLinkLocations =>

  lazy val ibus: InterruptBusWrapper = LazyModule(new InterruptBusWrapper)


  /**
   * Identity node that represents the full set of clock groups used by the subsystem.
   */
  lazy val allClockGroupsNode = ClockGroupIdentityNode()

  /**
   * Source node that represents externally-supplied clock groups when `SubsystemDriveClockGroupsFromIO` is enabled.
   */
  val clockSource = ClockGroupSourceNode(Seq(ClockGroupSourceParameters()))

  /**
   * IO ports for externally-driven clock groups, if `SubsystemDriveClockGroupsFromIO` is enabled.
   * Each port is a `ClockBundle` that directly drives the internal clock group graph through `allClockGroupsNode`.
   */
  val io_clocks: Option[ModuleValue[RecordMap[ClockBundle]]] =
    if (p(SubsystemDriveClockGroupsFromIO)) {
      Some(buildClockGroupIO())
    } else {
      None
    }

  case class BusInfo(idx: Int, name: String, member: String)

  case class IOClockPort(name: String, sink: ClockEdgeParameters)

  private lazy val clockBusPaths: Map[IOClockPort, IndexedSeq[BusInfo]] = {
    require(clockSource.out.size == 1, "ClockGroupSourceNode should have exactly one output port")
    val sourceEdge: ClockGroupEdgeParameters = clockSource.out.head._2

    // IO port member keys, indexed in the same order as sourceEdge.sink.members.
    // These become the field names in the io_clocks RecordMap (e.g. "aggregator_0").
    val ioPortNames: IndexedSeq[(String, ClockEdgeParameters)] = sourceEdge.members.toIndexedSeq

    // The ClockSinkParameters object for each IO port, by position.
    // These are the SAME Scala objects that each downstream clock sink
    // (device / bus) registered — reference equality is safe for matching.
    val ioClockSinkParams = sourceEdge.sink.members.toIndexedSeq
    require(ioPortNames.size == ioClockSinkParams.size)

    val busLocs = Seq[TLBusWrapperLocation](SBUS, COH, MBUS, CBUS, PBUS, FBUS)

    val allMatches: Seq[BusInfo] =
      for {
        loc <- busLocs
        bus <- tlBusWrapperLocationMap.get(loc).toSeq
        // bus.clockGroupNode.in gives inward edges of the bus aggregator
        // (edges from upstream clock providers: allClockGroupsNode or a
        // parent bus when DriveClocksFromSBus is enabled).
        (_, busEdge: ClockGroupEdgeParameters) <- bus.clockGroupNode.in
        busMemberNames = busEdge.members.keys.toIndexedSeq
        (busSink, busIdx) <- busEdge.sink.members.zipWithIndex
        (ioSink, ioIdx) <- ioClockSinkParams.zipWithIndex
        if ioSink eq busSink // Match the same ClockSinkParameters object by reference equality
      } yield BusInfo(ioIdx, bus.name, busMemberNames(busIdx))

    val busByIdx = allMatches.groupBy(_.idx)

    val clockMapping = mutable.LinkedHashMap.empty[IOClockPort, IndexedSeq[BusInfo]]
    ioPortNames.zipWithIndex.foreach { case ((name, edge), i: Int) =>
      busByIdx.get(i) match {
        case Some(busInfos) => clockMapping(IOClockPort(name, edge)) = busInfos.toIndexedSeq
        case None => clockMapping(IOClockPort(name, edge)) = IndexedSeq.empty
      }
    }
    clockMapping.toMap
  }

  def ioClockForBusLeaf(busLeaf: TLBusWrapperLocation): Option[IOClockPort] = {
    // Check the last element in each of the bus paths to find which one matches the given bus leaf location, then return the corresponding IOClockPort.
    clockBusPaths.collectFirst {
      case (ioPort, busInfos) if busInfos.lastOption.exists(_.name == busLeaf.name) => ioPort
    }
  }

  private def buildClockGroupIO(): ModuleValue[RecordMap[ClockBundle]] = {
    val aggregator = ClockGroupAggregator()

    // Connect externally-driven clock groups into the global clock
    // group graph so downstream consumers see no distinction between
    // internal and IO-sourced clocks.
    allClockGroupsNode :*= aggregator := clockSource

    InModuleBody {
      val elements = clockSource.out.flatMap { case (bundle, _) =>
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
      io
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