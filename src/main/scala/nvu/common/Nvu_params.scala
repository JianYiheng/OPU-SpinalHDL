package nvu.common

import spinal.core._
import spinal.lib._

object Nvu_params {
  var DATA_WIDTH     = 16
  var ELEM_NUMS      = 16

  val LUT_X_LEN      = 7
  val LUT_Y_LEN      = 7
  val LUT_K_LEN      = 6
  val LUT_X_INTG     = 6
  val LUT_X_FRAC     = 9
  val LUT_Y_INTG     = 6
  val LUT_Y_FRAC     = 9
  val LUT_K_INTG     = 6
  val LUT_K_FRAC     = 9
  val LUT_IN_INTG    = 6
  val LUT_IN_FRAC    = 9
  val LUT_OUT_INTG   = 6
  val LUT_OUT_FRAC   = 9
  val LUT_DELTA_INTG = 0
  val LUT_DELTA_FRAC = 15

  val MEAN_INTG      = 6
  val MEAN_FRAC      = 9
}

case class lut_if(d_intg: Int, d_frac: Int, d_len: Int) extends Bundle with IMasterSlave {
    val data = Vec(SFix (d_intg exp, -d_frac exp), d_len)
    
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

