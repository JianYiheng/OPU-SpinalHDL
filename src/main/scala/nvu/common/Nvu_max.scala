package nvu.common

import spinal.core._
import spinal.lib._

import spinal.core.sim._

import scala.util.Random
import scala.math
import scala.collection.mutable.Stack 

case class Nvu_max (elem_nums: Int) extends Component {
  val io = new Bundle {
    val x_vld = in Bool ()
    val y_vld = out Bool () setAsReg() init(False)
    val x_i   = in Vec(SInt(Nvu_params.DATA_WIDTH bits), elem_nums)
    val y_i   = out SInt(Nvu_params.DATA_WIDTH bits) setAsReg() init(0)
  }

  noIoPrefix()

  io.y_i := io.x_i.reduceBalancedTree(Max(_, _),(s,l)=>RegNext(s) init(0))
  io.y_vld := Delay(io.x_vld, elem_nums/2-1, init=False)

}

class Nvu_max_tb(elem_nums: Int) extends Nvu_max(elem_nums) {
  def init() {
    clockDomain.forkStimulus(10)
    io.x_vld #= false
    for (i <- 0 until elem_nums) {
      io.x_i(i) #= 0
    }
    sleep(100)
  }

  def source (): Stack[Array[Int]] = {
    val src: Stack[Array[Int]] = Stack()
    for (_ <- 0 until 100) {
      val src_ary = Seq.fill(elem_nums)(Random.nextInt(math.pow(2, Nvu_params.DATA_WIDTH-1).toInt-1)).toArray
      src.push(src_ary)
    }

    return src
  }

  def driveAndCheck (src: Stack[Array[Int]]) {
    val src_drv, src_chk = src.clone

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

    val checker_task = fork {
      while (src_chk.length>0) {
        if (io.y_vld.toBoolean) {
          val src_ary = src_chk.pop()
          assert(io.y_i.toInt == src_ary.max, s"y_i: ${io.y_i.toInt}, max: ${src_ary.max}")
        }
        clockDomain.waitSampling()
      }
      simSuccess()
    }

    driver_task.join()
    checker_task.join()
  }
}

object Nvu_max_main{
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(Nvu_max(elem_nums=8))
  }
}
