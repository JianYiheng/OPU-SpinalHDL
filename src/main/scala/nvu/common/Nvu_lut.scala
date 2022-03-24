package nvu.common

import spinal.core._
import spinal.lib._

class Nvu_lut extends Component {
  val io = new Bundle {
    val lut_x = in Vec(SFix(Nvu_params.LUT_X_INTG exp, -Nvu_params.LUT_X_FRAC exp), Nvu_params.LUT_X_LEN)
    val lut_y = in Vec(SFix(Nvu_params.LUT_Y_INTG exp, -Nvu_params.LUT_Y_FRAC exp), Nvu_params.LUT_Y_LEN)
    val lut_k = in Vec(SFix(Nvu_params.LUT_K_INTG exp, -Nvu_params.LUT_K_FRAC exp), Nvu_params.LUT_X_LEN)

    val x_i   = in SFix(Nvu_params.LUT_IN_INTG exp, -Nvu_params.LUT_IN_FRAC exp)
    val y_i   = out SFix(Nvu_params.LUT_OUT_INTG exp, -Nvu_params.LUT_OUT_FRAC exp)
  }

  val lut_pos_ary  = Reg(Bits(Nvu_params.LUT_X_LEN bits)) init(0)
  val lut_pos      = CountOne(lut_pos_ary) - 1

  val lut_x_p      = io.lut_x(lut_pos)
  val lut_y_p      = io.lut_y(lut_pos)
  val lut_y_p_1    = io.lut_y(lut_pos+1)
  val lut_k_p      = io.lut_k(lut_pos)

  val delta_one    = Reg(SFix(Nvu_params.LUT_DELTA_INTG exp, -Nvu_params.LUT_DELTA_FRAC exp)) init (1)
  val delta_minus  = Reg(SFix(Nvu_params.LUT_DELTA_INTG exp, -Nvu_params.LUT_DELTA_FRAC exp)) init (0)
  val delta        = Reg(SFix(Nvu_params.LUT_DELTA_INTG exp, -Nvu_params.LUT_DELTA_FRAC exp)) init (0)

  for (i <- 0 until Nvu_params.LUT_X_LEN) {
    when (io.x_i > io.lut_x(i)) {
      lut_pos_ary(i)  := True
    }.otherwise {
      lut_pos_ary(i)  := False
    }
  }

  delta       := ((io.x_i - lut_x_p) * lut_k_p).truncated
  delta_minus := (delta_one - delta).truncated

  io.y_i := (delta * lut_y_p + delta_minus * lut_y_p_1).truncated

}

object Nvu_lut_main {
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(Nvu_lut)
  }
}

