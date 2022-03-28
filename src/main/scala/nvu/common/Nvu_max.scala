package nvu.common

import spinal.core._
import spinal.lib._

import spinal.core.sim._

import scala.util.Random
import scala.math
import scala.collection.mutable.Stack 

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

class Nvu_max_tb(elem_nums: Int) extends Nvu_max(elem_nums) {
  def init() {
    clockDomain.forkStimulus(10)
    for (i <- 0 until elem_nums) {
      io.x_i(i) #= 0
    }
    clockDomain.waitSampling()
  }

  def source (): Stack[Array[Int]] = {
    val src: Stack[Array[Int]] = Stack()
    for (_ <- 0 until 100) {
      val src_ary = Seq.fill(elem_nums)(Random.nextInt(math.pow(2, Nvu_params.DATA_WIDTH-1).toInt-1)).toArray
      src.push(src_ary)
    }

    return src
  }

  def driver (src: Stack[Array[Int]]) {
    while(src.length>0) {
      val src_ary = src.pop()
      for (i <- 0 until src_ary.length) {
        io.x_i(i) #= src_ary(i)
      }
      clockDomain.waitSampling()
    }
  }

  def checker (src: Stack[Array[Int]]) {
    clockDomain.waitSampling ()
    while (src.length>0) {
      val src_ary = src.pop()
      clockDomain.waitSampling()
      assert(io.y_i.toInt == src_ary.max, "y_i: $(io.y_i.toInt), max: ${src_ary.max}")
    }
  }
}

object Nvu_max_main{
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(Nvu_max(elem_nums=8))
  }
}
