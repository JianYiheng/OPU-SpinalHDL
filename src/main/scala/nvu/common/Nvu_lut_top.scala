package nvu.common

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.util.Random
import scala.math
import scala.collection.mutable.Stack

import spinal.lib.dsptool.FixData

case class Nvu_lut_top () extends Component {
  val io = new Bundle {
    val sel = in UInt (2 bits)
    val x_vld = in Bool ()
    val y_vld = out Bool ()
    val x_i = in SFix(Nvu_params.LUT_IN_INTG exp, -Nvu_params.LUT_IN_FRAC exp)
    val y_i = out SFix(Nvu_params.LUT_OUT_INTG exp, -Nvu_params.LUT_OUT_FRAC exp)
  }

  noIoPrefix()

  val nvu_lut_inst = new Nvu_lut ()
  val nvu_lut_rf_inst = new Nvu_lut_rf ()

  val sel_dly = RegNext(io.sel)
  val x_vld_dly = RegNext(io.x_vld)
  val x_i_dly = RegNext(io.x_i)

  nvu_lut_rf_inst.io.ren <> io.x_vld
  nvu_lut_rf_inst.io.raddr <> io.sel

  nvu_lut_rf_inst.io.lut_x <> nvu_lut_inst.io.lut_x
  nvu_lut_rf_inst.io.lut_y <> nvu_lut_inst.io.lut_y
  nvu_lut_rf_inst.io.lut_k <> nvu_lut_inst.io.lut_k

  x_vld_dly <> nvu_lut_inst.io.x_vld
  x_i_dly <> nvu_lut_inst.io.x_i

  io.y_vld <> nvu_lut_inst.io.y_vld
  io.y_i <> nvu_lut_inst.io.y_i

}

class Nvu_lut_top_tb extends Nvu_lut_top {
  def init () {
    clockDomain.forkStimulus(10)
  }  
}

object Nvu_lut_top_main {
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(Nvu_lut_top())
  }
}

