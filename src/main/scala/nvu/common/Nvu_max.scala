package nvu.common

import spinal.core._
import spinal.lib._

case class Nvu_max (elem_nums: Int) extends Component {
  val io = new Bundle {
    val x_i = in Vec(SInt(Nvu_params.DATA_WIDTH bits), elem_nums)
    val y_i = out SInt(Nvu_params.DATA_WIDTH bits)
  }

  val x_a = Reg(SInt(Nvu_params.DATA_WIDTH bits))
  val x_b = Reg(SInt(Nvu_params.DATA_WIDTH bits))

  if (elem_nums == 2) {
    x_a := io.x_i(0)
    x_b := io.x_i(1)
  } else {
    val sub_Nvu_max_a = new Nvu_max (elem_nums=elem_nums/2)
    val sub_Nvu_max_b = new Nvu_max (elem_nums=elem_nums/2)

    for (i <- 0 until elem_nums/2) {
      sub_Nvu_max_a.io.x_i(i) := io.x_i(i)
      sub_Nvu_max_b.io.x_i(i) := io.x_i(i+elem_nums/2)
    }

    x_a := sub_Nvu_max_a.io.y_i
    x_b := sub_Nvu_max_b.io.y_i
  }

  when (x_a > x_b) {
    io.y_i := x_a
  }.otherwise {
    io.y_i := x_b
  }
}

object Nvu_max_main{
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(Nvu_max(elem_nums=16))
  }
}
