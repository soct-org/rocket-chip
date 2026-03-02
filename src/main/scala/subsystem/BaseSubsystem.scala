// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import chisel3._
import freechips.rocketchip.prci.{ClockBundle, ClockGroupAggregator, ClockGroupIdentityNode, ClockGroupSourceNode, ClockGroupSourceParameters}
import freechips.rocketchip.resources.{BindingScope, DTB, DTS, JSON}
import freechips.rocketchip.tilelink.{TLBusWrapper, TLBusWrapperTopology}
import freechips.rocketchip.util.{ElaborationArtefacts, Location, PlusArgArtefacts, RecordMap}
import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

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
   * Source node that represents externally-supplied clock groups from the environment, if [[SubsystemDriveClockGroupsFromIO]] is enabled.
   */
  val clockSource: Option[ClockGroupSourceNode] =
    if (p(SubsystemDriveClockGroupsFromIO)) {
      Some(ClockGroupSourceNode(Seq(ClockGroupSourceParameters())))
    } else {
      None
    }

  /**
   * IO ports for externally-driven clock groups, if [[SubsystemDriveClockGroupsFromIO]] is enabled.
   * Each port is a `ClockBundle` that directly drives the internal clock group graph through `allClockGroupsNode`.
   */
  val io_clocks: Option[ModuleValue[RecordMap[ClockBundle]]] =
    if (p(SubsystemDriveClockGroupsFromIO)) {
      Some(buildClockGroupIO())
    } else {
      None
    }

  /**
   * A mapping of clock bundles from the [[clockSource]] to the set of TLBusWrappers that are driven by those clock bundles.
   * Returns an empty map if [[SubsystemDriveClockGroupsFromIO]] is disabled.
   */
  lazy val busHierarchyByClock: Map[ClockBundle, Set[TLBusWrapper]] = clockSource.fold(Map.empty[ClockBundle, Set[TLBusWrapper]]) { source =>
    val topoInSubsystem = p(TLNetworkTopologyLocated(InSubsystem))
    val busTopos: Seq[TLBusWrapperTopology] = topoInSubsystem.collect { case t: TLBusWrapperTopology => t }

    val busHierMap = mutable.LinkedHashMap.empty[ClockBundle, mutable.LinkedHashSet[TLBusWrapper]]

    source.out.foreach { case (_, sourceEdge) => // Should only be one iteration.
      sourceEdge.members.foreach { case (portName, sourceForBus) =>
        val clk = io_clocks.get.getWrappedValue.elements.getOrElse(portName, throw new RuntimeException(s"Clock bundle \"$portName\" not found in io_clocks RecordMap"))
        val buses = mutable.LinkedHashSet.empty[TLBusWrapper]

        def connectsToSource(busInst: TLBusWrapper): Boolean = {
          // get in edges of the bus and find which one matches this sink parameter
          val busInEdges = busInst.clockGroupNode.in.map(_._2).flatMap(_.sink.members)
          busInEdges.exists { busSink => busSink eq sourceForBus.sink }
        }

        busTopos.foreach { topo =>
          topo.instantiations.foreach { case (loc, _) =>
            val busInst = locateTLBusWrapper(loc)
            if (connectsToSource(busInst)) {
              buses += busInst
            }
          }
        }

        busHierMap += (clk -> buses)
      }
    }
    busHierMap.map { case (k, v) => k -> v.toSet }.toMap
  }

  /**
   * Returns the [[ClockBundle]] from [[io_clocks]] that drives the given [[TLBusWrapper]], if one exists.
   *
   * This can be used to find the clock port associated with any TL bus in the subsystem topology,
   * including buses reachable through multi-branch clock hierarchies (e.g. mbus and pbus both
   * driven by the sbus clock). Returns [[None]] if [[SubsystemDriveClockGroupsFromIO]] is disabled
   * or if the bus is not found in the clock hierarchy.
   */
  def clockBundleForBus(bus: TLBusWrapper): Option[ClockBundle] =
    busHierarchyByClock.collectFirst { case (clk, buses) if buses.contains(bus) => clk }

  private def buildClockGroupIO(): ModuleValue[RecordMap[ClockBundle]] = {
    val aggregator = ClockGroupAggregator()

    // Connect externally-driven clock groups into the global clock
    // group graph so downstream consumers see no distinction between
    // internal and IO-sourced clocks.
    allClockGroupsNode :*= aggregator := clockSource.get

    InModuleBody {
      val elements = clockSource.get.out.flatMap { case (bundle, _) =>
        bundle.member.elements
      }
      val io = clockGroupIO(elements)

      // Mechanically wire each IO clock bundle to its corresponding
      // internal clock group member.
      elements.foreach { case (name, internalPort) =>
        io(name).foreach { ioPort =>
          internalPort := ioPort
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
        params = data.params, resetType = Some(resetType)
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