package nvu.common

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.util.Random
import scala.math
import scala.collection.mutable.Stack 

import spinal.lib.dsptool.FixData

case class Nvu_mean(elem_nums: Int) extends Component {
  val io = new Bundle {
    val x_vld = in Bool ()
    val y_vld = out Bool ()
    val pkg_done = in Bool ()

    val x_i = in Vec(SInt(Nvu_params.DATA_WIDTH bits), elem_nums)
    val y_i = out SInt(Nvu_params.DATA_WIDTH bits) setAsReg() init(0)
  }

  noIoPrefix()

  var log2  = (x: Double) => scala.math.log10(x)/scala.math.log10(2.0)
  val x_sum = SInt(Nvu_params.DATA_WIDTH+log2(elem_nums).toInt bits)

  x_sum    := io.x_i.reduceBalancedTree(_+^_, (s, l)=>RegNext(s))
  io.y_i   := RegNext((x_sum >> 2).fixTo(15 downto 0))
  io.y_vld := Delay(io.x_vld, 4, init=False)
}

class Nvu_main_tb(elem_nums: Int) extends Nvu_mean(elem_nums) {
  def init () {
    clockDomain.forkStimulus(10)
    sleep(100)
    io.x_vld #= false
  }

  def source () {
  }

  def driveAndCheck (src: Stack[Int]) {
    val src_drv = src.clone
    val src_chk = src.clone

    val driver_task = fork {
      clockDomain.waitSampling(2)
      while(src_drv.length>0) {
        if (Random.nextFloat() > 0.5) {
          io.x_vld #= false
        } else {
          val src_ary = src_drv.pop()
          io.x_vld #= true
          for (i <- 0 until elem_nums) {
            io.x_i(i) #= src_ary(i)
          }
        }
        clockDomain.waitSampling()
      }
    }
  }
}

object Nvu_mean_main{
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(Nvu_mean(elem_nums=8))
  }
}
