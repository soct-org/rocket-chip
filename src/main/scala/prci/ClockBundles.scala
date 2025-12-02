// See LICENSE.SiFive for license details.
package freechips.rocketchip.prci

import chisel3._
import freechips.rocketchip.util.RecordMap


class ClockBundle(
                   val params: ClockBundleParameters = ClockBundleParameters(),
                   val resetType: Option[() => Reset] = None
                 ) extends Bundle {
  val clock = Output(Clock())
  val reset = if (resetType.isDefined) Output(resetType.get()) else Output(Reset())
}

class ClockGroupBundle(val params: ClockGroupBundleParameters) extends Bundle {
  val member: RecordMap[ClockBundle] = RecordMap(params.members.map { case (k, v) =>
    k -> new ClockBundle(v)
  })
}
