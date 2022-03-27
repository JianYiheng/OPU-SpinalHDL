package nvu.common

import spinal.core._
import spinal.lib._

import scala.util.Random
import scala.math

case class Nvu_lut() extends Component {
  val io = new Bundle {
    val x_vld = in Bool()
    val y_vld = out Bool()

    val lut_x = in Vec(SFix(Nvu_params.LUT_X_INTG exp, -Nvu_params.LUT_X_FRAC exp), Nvu_params.LUT_X_LEN)
    val lut_y = in Vec(SFix(Nvu_params.LUT_Y_INTG exp, -Nvu_params.LUT_Y_FRAC exp), Nvu_params.LUT_Y_LEN)
    val lut_k = in Vec(SFix(Nvu_params.LUT_K_INTG exp, -Nvu_params.LUT_K_FRAC exp), Nvu_params.LUT_X_LEN)

    val x_i   = in SFix(Nvu_params.LUT_IN_INTG exp, -Nvu_params.LUT_IN_FRAC exp)
    val y_i   = out SFix(Nvu_params.LUT_OUT_INTG exp, -Nvu_params.LUT_OUT_FRAC exp)
  }

  val lut_pos_ary = Reg(Bits(Nvu_params.LUT_X_LEN bits))
  val lut_pos     = CountOne(lut_pos_ary)-1

  val lut_x_r     = Reg(SFix(Nvu_params.LUT_X_INTG exp, -Nvu_params.LUT_X_FRAC exp))
  val lut_y_r     = Reg(SFix(Nvu_params.LUT_Y_INTG exp, -Nvu_params.LUT_Y_FRAC exp))
  val lut_y_r_1   = Reg(SFix(Nvu_params.LUT_Y_INTG exp, -Nvu_params.LUT_Y_FRAC exp))
  val lut_k_r     = Reg(SFix(Nvu_params.LUT_K_INTG exp, -Nvu_params.LUT_K_FRAC exp))

  val delta       = SFix(Nvu_params.LUT_DELTA_INTG exp, -Nvu_params.LUT_DELTA_FRAC exp)
  val delta_one   = SFix(Nvu_params.LUT_DELTA_INTG+1 exp, -Nvu_params.LUT_DELTA_FRAC exp)
  val delta_a     = Reg(SFix(Nvu_params.LUT_DELTA_INTG exp, -Nvu_params.LUT_DELTA_FRAC exp))
  val delta_b     = Reg(SFix(Nvu_params.LUT_DELTA_INTG exp, -Nvu_params.LUT_DELTA_FRAC exp))

  when (io.x_vld) {
    for (i <- 0 until Nvu_params.LUT_X_LEN) {
      when (io.x_i > io.lut_x(i)) {
        lut_pos_ary(i)  := True
      }.otherwise {
        lut_pos_ary(i)  := False
      }
    }
  }

  when (Delay(io.x_vld, 1)) {
    lut_x_r   := io.lut_x(lut_pos)
    lut_y_r   := io.lut_y(lut_pos)
    lut_y_r_1 := io.lut_y(lut_pos+1)
    lut_k_r   := io.lut_k(lut_pos)
  }.otherwise {
    lut_x_r   := lut_x_r
    lut_y_r   := lut_y_r
    lut_y_r_1 := lut_y_r_1
    lut_k_r   := lut_k_r
  }

  delta     := ((Delay(io.x_i, 2) - lut_x_r) * lut_k_r).truncated
  delta_one := 1

  when (Delay(io.x_vld, 2)) {
    delta_a := delta
    delta_b := (delta_one - delta).truncated
  }

  io.y_i := Vec(delta_a, delta_b).zip(Vec(lut_y_r, lut_y_r_1)).map{case(a, b)=>RegNext(a * b)}.reduceBalancedTree(_+_,(s,l)=>RegNext(s)).truncated

  io.y_vld := Delay(io.x_vld, 6)

}

object Nvu_lut_main {
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(Nvu_lut())
  }
}

