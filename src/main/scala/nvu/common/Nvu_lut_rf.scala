package nvu.common

import spinal.core._
import spinal.lib._

case class lut_data(d_intg: Int, d_frac: Int, d_len: Int) extends Bundle with IMasterSlave {
    val data = Vec(SFix (d_intg exp, -d_frac exp), 3)
    
    def initValue (dataValue: Seq[Double]): this.type = {
        for (i <- 0 until d_len) {
            data(i) := dataValue(i)
        }
        this
    }
    override def asMaster(): Unit = {
        out (data)
    }
}

case class Nvu_lut_rf() extends Component {
  val io = new Bundle {
    val sel = in Bits (3 bits)
    
    val lut_x = out Vec (SFix(Nvu_params.LUT_X_INTG exp, Nvu_params.LUT_X_FRAC exp), Nvu_params.LUT_X_LEN)
    val lut_y = out Vec (SFix(Nvu_params.LUT_Y_INTG exp, Nvu_params.LUT_Y_FRAC exp), Nvu_params.LUT_Y_LEN)
    val lut_k = out Vec (SFix(Nvu_params.LUT_K_INTG exp, Nvu_params.LUT_K_FRAC exp), Nvu_params.LUT_K_LEN)
  }

  val sf_lut_x = Seq (-6, -5, -4, -3, -2, -1, 0)
  val sf_lut_y = Seq (0.0024787521766663585, 0.006737946999085467, 0.01831563888873418, 0.049787068367863944, 0.1353352832366127, 0.36787944117144233, 1)
  val sf_lut_k = Seq (1.0, 1.0, 1.0, 1.0, 1.0, 1.0)

  val ln_lut_x = Seq ()
  val ln_lut_y = Seq ()
  val ln_lut_k = Seq ()

  val gl_lut_x = Seq ()
  val gl_lut_y = Seq ()
  val gl_lut_k = Seq ()

  val mem_x = Mem(lut_data(Nvu_params.LUT_X_INTG, Nvu_params.LUT_X_FRAC, Nvu_params.LUT_X_LEN), 3)
  val mem_y = Mem(lut_data(Nvu_params.LUT_Y_INTG, Nvu_params.LUT_Y_FRAC, Nvu_params.LUT_Y_LEN), 3)
  val mem_k = Mem(lut_data(Nvu_params.LUT_K_INTG, Nvu_params.LUT_Y_FRAC, Nvu_params.LUT_K_LEN), 3)

  mem_x.init (
    Seq (
      lut_data().initValue(sf_lut_x),
      lut_data().initValue(ln_lut_x),
      lut_data().initValue(gl_lut_x))
  )

  mem_y.init (
    Seq (
      lut_data().initValue (sf_lut_y),
      lut_data().initValue (ln_lut_y),
      lut_data().initValue (gl_lut_y))
  )

  mem_k.init (
    Seq (
      lut_data().initValue (sf_lut_k),
      lut_data().initValue (ln_lut_k),
      lut_data().initValue (gl_lut_k))
  )

  io.lut_x := mem_x.readSync(io.raddr, io.ren)
  io.lut_y := mem_y.readSync(io.raddr, io.ren)
  io.lut_k := mem_k.readSync(io.raddr, io.ren)
}

object Nvu_lut_rf_main {
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(new Nvu_lut_rf)
  }
}
