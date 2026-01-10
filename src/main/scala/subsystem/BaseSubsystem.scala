// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.diplomacy.AddressRange
import freechips.rocketchip.resources.{AddressMapEntry, BindingScope, DTB, DTS, DTSCompat, DTSModel, DTSTimebase, JSON, Resource, ResourceAnchors, ResourceBinding, ResourceInt, ResourceString}
import freechips.rocketchip.prci.{ClockBundle, ClockGroupAggregator, ClockGroupIdentityNode, ClockGroupSourceNode, ClockGroupSourceParameters}
import freechips.rocketchip.tilelink.TLBusWrapper
import freechips.rocketchip.util.{ElaborationArtefacts, Location, PlusArgArtefacts, RecordMap}

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

/** BareSubsystem is the root class for creating a subsystem */
abstract class BareSubsystem(implicit p: Parameters) extends LazyModule

abstract class BareSubsystemModuleImp[+L <: BareSubsystem](_outer: L) extends LazyRawModuleImp(_outer)

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
 */
trait HasConfigurablePRCILocations {
  this: HasPRCILocations =>

  // Interrupt bus is always present; unrelated to clocking policy
  val ibus: InterruptBusWrapper = LazyModule(new InterruptBusWrapper)

  // Identity node that represents the full set of clock groups
  // used by the subsystem. Policy determines how these clocks
  // are sourced; mechanisms below implement that policy.
  val allClockGroupsNode = ClockGroupIdentityNode()


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
   *     2. Aggregating those clocks so they participate in the unified clock group graph.
   *     3. Materializing IO ports that directly drive the internal ClockBundle instances.
   *
   * No clocking decisions are made here; this is purely a structural
   * realization of the policy selected above.
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

/** Base Subsystem class with no peripheral devices, ports or cores added yet */
abstract class BaseSubsystem(val location: HierarchicalLocation = InSubsystem)
                            (implicit p: Parameters)
  extends BareSubsystem
    with HasDTS
    with Attachable
    with HasConfigurablePRCILocations
    with HasConfigurableTLNetworkTopology {
  override val module: BaseSubsystemModuleImp[BaseSubsystem]

  val busContextName = "subsystem"

  viewpointBus.clockGroupNode := allClockGroupsNode

  // TODO: Preserve legacy implicit-clock behavior for IBUS for now. If binding
  //       a PLIC to the CBUS, ensure it is synchronously coupled to the SBUS.
  ibus.clockNode := viewpointBus.fixedClockNode

  // Collect information for use in DTS
  ResourceBinding {
    val managers = topManagers
    val max = managers.flatMap(_.address).map(_.max).max
    val width = ResourceInt((log2Ceil(max) + 31) / 32)
    val model = p(DTSModel)
    val compat = p(DTSCompat)
    var hertz = p(DTSTimebase) // add for timebase-frequency
    val devCompat = (model +: compat).map(s => ResourceString(s + "-dev"))
    val socCompat = (model +: compat).map(s => ResourceString(s + "-soc"))
    devCompat.foreach {
      Resource(ResourceAnchors.root, "compat").bind(_)
    }
    socCompat.foreach {
      Resource(ResourceAnchors.soc, "compat").bind(_)
    }
    Resource(ResourceAnchors.root, "model").bind(ResourceString(model))
    Resource(ResourceAnchors.root, "width").bind(width)
    Resource(ResourceAnchors.soc, "width").bind(width)
    Resource(ResourceAnchors.cpus, "width").bind(ResourceInt(1))
    Resource(ResourceAnchors.cpus, "hertz").bind(ResourceInt(hertz))

    managers.foreach { case manager =>
      val value = manager.toResource
      manager.resources.foreach { case resource =>
        resource.bind(value)
      }
    }
  }
}


abstract class BaseSubsystemModuleImp[+L <: BaseSubsystem](_outer: L) extends BareSubsystemModuleImp(_outer) with HasDTSImp[L] {
  def dtsLM: L = _outer

  private val mapping: Seq[AddressMapEntry] = {
    dtsLM.collectResourceAddresses.groupBy(_._2).toList.flatMap { case (key, seq) =>
      AddressRange.fromSets(key.address).map { r => AddressMapEntry(r, key.permissions, seq.map(_._1)) }
    }.sortBy(_.range)
  }

  println("Generated Address Map")
  mapping.foreach(entry => println(entry.toString((dtsLM.tlBusWrapperLocationMap(p(TLManagerViewpointLocated(dtsLM.location))).busView.bundle.addressBits - 1) / 4 + 1)))
  println("")

  ElaborationArtefacts.add("memmap.json", s"""{"mapping":[${mapping.map(_.toJSON).mkString(",")}]}""")

  // Confirm that all of memory was described by DTS
  private val dtsRanges = AddressRange.unify(mapping.map(_.range))
  private val allRanges = AddressRange.unify(dtsLM.topManagers.flatMap { m => AddressRange.fromSets(m.address) })

  if (dtsRanges != allRanges) {
    println("Address map described by DTS differs from physical implementation:")
    AddressRange.subtract(allRanges, dtsRanges).foreach { case r =>
      println(s"\texists, but undescribed by DTS: ${r}")
    }
    AddressRange.subtract(dtsRanges, allRanges).foreach { case r =>
      println(s"\tdoes not exist, but described by DTS: ${r}")
    }
    println("")
  }
}
