// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import org.chipsalliance.cde.config._

import freechips.rocketchip.devices.debug.HasPeripheryDebug
import freechips.rocketchip.devices.tilelink.{CanHavePeripheryCLINT, CanHavePeripheryPLIC}
import freechips.rocketchip.prci.{ResetCrossingType, NoResetCrossing, SynchronousCrossing, ClockCrossingType}
import freechips.rocketchip.tile.{RocketTile, RocketTileParams}
import freechips.rocketchip.util.HasCoreMonitorBundles

case class RocketCrossingParams(
                                 crossingType: ClockCrossingType = SynchronousCrossing(),
                                 master: HierarchicalElementPortParamsLike = HierarchicalElementMasterPortParams(),
                                 slave: HierarchicalElementSlavePortParams = HierarchicalElementSlavePortParams(),
                                 mmioBaseAddressPrefixWhere: TLBusWrapperLocation = CBUS,
                                 resetCrossingType: ResetCrossingType = NoResetCrossing(),
                                 forceSeparateClockReset: Boolean = false
                               ) extends HierarchicalElementCrossingParamsLike

case class RocketTileAttachParams(
                                   tileParams: RocketTileParams,
                                   crossingParams: RocketCrossingParams
                                 ) extends CanAttachTile {
  type TileType = RocketTile
}

trait HasRocketTiles {
  this: InstantiatesHierarchicalElements =>
  val rocketTiles = totalTiles.values.collect { case r: RocketTile => r }

  def coreMonitorBundles = (rocketTiles map { t =>
    t.module.core.rocketImpl.coreMonitorBundle
  }).toList
}


