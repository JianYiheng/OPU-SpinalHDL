package nvu.common

import spinal.core._
import spinal.lib._

case class Nvu_lut_rf() extends Component {
  val io = new Bundle {
    val sel = in Bits (3 bits)
    
    // val lut_x = out Vec (SFix(Nvu_params.LUT_X_INTG exp, Nvu_params.LUT_X_FRAC exp), Nvu_params.LUT_X_LEN)
    // val lut_y = out Vec (SFix(Nvu_params.LUT_Y_INTG exp, Nvu_params.LUT_Y_FRAC exp), Nvu_params.LUT_Y_LEN)
    // val lut_k = out Vec (SFix(Nvu_params.LUT_K_INTG exp, Nvu_params.LUT_K_FRAC exp), Nvu_params.LUT_K_LEN)
    val lut_x = out Vec (UInt(Nvu_params.DATA_WIDTH bits), Nvu_params.LUT_X_LEN)
    val lut_y = out Vec (UInt(Nvu_params.DATA_WIDTH bits), Nvu_params.LUT_Y_LEN)
    val lut_k = out Vec (UInt(Nvu_params.DATA_WIDTH bits), Nvu_params.LUT_K_LEN)
  }

  // val sf_lut_x = Vec (SFix(Nvu_params.LUT_X_INTG exp, Nvu_params.LUT_X_FRAC exp), Nvu_params.LUT_X_LEN).setAsReg()
  // val sf_lut_y = Vec (SFix(Nvu_params.LUT_Y_INTG exp, Nvu_params.LUT_Y_FRAC exp), Nvu_params.LUT_Y_LEN).setAsReg()
  // val sf_lut_k = Vec (SFix(Nvu_params.LUT_K_INTG exp, Nvu_params.LUT_K_FRAC exp), Nvu_params.LUT_K_LEN).setAsReg()

  // val ln_lut_x = Vec (SFix(Nvu_params.LUT_X_INTG exp, Nvu_params.LUT_X_FRAC exp), Nvu_params.LUT_X_LEN).setAsReg()
  // val ln_lut_y = Vec (SFix(Nvu_params.LUT_Y_INTG exp, Nvu_params.LUT_Y_FRAC exp), Nvu_params.LUT_Y_LEN).setAsReg()
  // val ln_lut_k = Vec (SFix(Nvu_params.LUT_K_INTG exp, Nvu_params.LUT_K_FRAC exp), Nvu_params.LUT_K_LEN).setAsReg()

  // val gl_lut_x = Vec (SFix(Nvu_params.LUT_X_INTG exp, Nvu_params.LUT_X_FRAC exp), Nvu_params.LUT_X_LEN).setAsReg()
  // val gl_lut_y = Vec (SFix(Nvu_params.LUT_Y_INTG exp, Nvu_params.LUT_Y_FRAC exp), Nvu_params.LUT_Y_LEN).setAsReg()
  // val gl_lut_k = Vec (SFix(Nvu_params.LUT_K_INTG exp, Nvu_params.LUT_K_FRAC exp), Nvu_params.LUT_K_LEN).setAsReg()

  val sf_lut_x = Vec(Seq(0x0000, 0xfe00, 0xfc00, 0xfa00, 0xf386, 0xf386, 0xf386).map(U(_,Nvu_params.DATA_WIDTH bits))) setAsReg()
  val sf_lut_y = Vec(Seq(0x0200, 0x00bc, 0x0045, 0x0019, 0x0001, 0x0001, 0x0001, 0x0001).map(U(_,Nvu_params.DATA_WIDTH bits))) setAsReg()
  val sf_lut_k = Vec(Seq(0x0800, 0x0800, 0x0800, 0x0800, 0x0278, 0x0278, 0x0278).map(U(_,Nvu_params.DATA_WIDTH bits))) setAsReg()

  val ln_lut_x = Vec(Seq(0x8000, 0x5d20, 0x428f, 0x37e1, 0x2eaa, 0x26c2, 0x2000).map(U(_,Nvu_params.DATA_WIDTH bits))) setAsReg()
  val ln_lut_y = Vec(Seq(0x4000, 0x4b08, 0x58c0, 0x60dc, 0x69fe, 0x744e, 0x8000, 0x8000).map(U(_,Nvu_params.DATA_WIDTH bits))) setAsReg()
  val ln_lut_k = Vec(Seq(0x0000, 0x1d5d, 0x268b, 0x5fe0, 0x6f27, 0x7fff, 0x7fff).map(U(_,Nvu_params.DATA_WIDTH bits))) setAsReg()

  val gl_lut_x = Vec(Seq(0xfa00, 0xfe00, 0x0000, 0x0200, 0x0600, 0x0600, 0x0600).map(U(_,Nvu_params.DATA_WIDTH bits))) setAsReg()
  val gl_lut_y = Vec(Seq(0xfffe, 0xffaf, 0x0000, 0x01af, 0x0000, 0x0000, 0x0000, 0x0000).map(U(_,Nvu_params.DATA_WIDTH bits))) setAsReg()
  val gl_lut_k = Vec(Seq(0xff62, 0x0145, 0x06bb, 0x089e, 0x0800, 0x0800, 0x0800).map(U(_,Nvu_params.DATA_WIDTH bits))) setAsReg()

  switch(io.sel) {
    is(0) {
      io.lut_x := sf_lut_x
      io.lut_y := sf_lut_y
      io.lut_k := sf_lut_k
    }
    is(1) {
      io.lut_x := ln_lut_x
      io.lut_y := ln_lut_y
      io.lut_k := ln_lut_k
    }
    is(2) {
      io.lut_x := gl_lut_x
      io.lut_y := gl_lut_y
      io.lut_k := gl_lut_k
    }
    default {
      io.lut_x := sf_lut_x
      io.lut_y := sf_lut_y
      io.lut_k := sf_lut_k
    }
  }
}

object Nvu_lut_rf_main {
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(new Nvu_lut_rf)
  }
}
