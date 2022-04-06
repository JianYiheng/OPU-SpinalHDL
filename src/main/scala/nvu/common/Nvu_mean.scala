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

    val x_i = in Vec(AFix(Nvu_params.MEAN_INTG exp, -Nvu_params.MEAN_FRAC exp, true), elem_nums)
    val y_i = out SFix(Nvu_params.MEAN_INTG exp, -Nvu_params.MEAN_FRAC exp) setAsReg() init(0)
  }

  noIoPrefix()

  var log2  = (x: Int) => (scala.math.log10(x)/scala.math.log10(2.0)).toInt
  val shf_len = log2(elem_nums).toInt

  io.y_i := (io.x_i.reduceBalancedTree(_+_, (s, l)=>RegNext(s)).asSFix >> shf_len).truncated
  io.y_vld := Delay(io.x_vld, shf_len, init=False)
}

class Nvu_mean_tb(elem_nums: Int) extends Nvu_mean(elem_nums) {
  def init () {
    clockDomain.forkStimulus(10)
    sleep(100)
    io.x_vld #= false
  }

  def source (): Stack[Array[Double]] = {
    // val maxValue = BigInt(2).pow(Nvu_params.MEAN_INTG+1)-BigInt(2).pow(Nvu_params.MEAN_FRAC)
    // val minValue = -BigInt(2).pow(Nvu_params.MEAN_INTG)

    val maxValue = scala.math.pow(2, Nvu_params.MEAN_INTG+1) - scala.math.pow(2, -Nvu_params.MEAN_FRAC)
    val minValue = -scala.math.pow(2, Nvu_params.MEAN_INTG)
    val src: Stack[Array[Double]] = Stack()
    for (_ <- 0 until 100) {
      val src_ary = Seq.fill(elem_nums)(Random.nextDouble()*(maxValue-minValue).toLong+minValue.toLong).toArray
      src.push(src_ary)
    }

    return src
  }

  def driveAndCheck (src: Stack[Array[Double]]) {
    val src_drv, src_chk = src.clone

    val driver_task = fork {
      clockDomain.waitSampling(2)
      while(src_drv.length>0) {
        if (Random.nextFloat() > 0.5) {
          io.x_vld #= false
        } else {
          val src_ary: Array[Double] = src_drv.pop()
          io.x_vld #= true
          for (i <- 0 until elem_nums) {
            // io.x_i(i).raw #= FixData(src_ary(i), SQ(Nvu_params.DATA_WIDTH, Nvu_params.MEAN_FRAC)).asLong
            io.x_i(i).raw #= (src_ary(i) * scala.math.pow(2, Nvu_params.MEAN_FRAC)).toInt
          }
        }
        clockDomain.waitSampling()
      }
    }

    val checker_task = fork {
      clockDomain.waitSampling(2)
      while (src_chk.length>0) {
        if (io.y_vld.toBoolean) {
          val data_src: Array[Double] = src_chk.pop()
          val data_ref: Double = data_src.sum / data_src.size
          val data_rev: Double = io.y_i.raw.toInt/math.pow(2, Nvu_params.LUT_OUT_FRAC)
          assert (((data_rev - data_ref)/data_ref).abs < 0.3, "mean error")
        }
        clockDomain.waitSampling()
      }
    }

    driver_task.join()
    checker_task.join()
  }
}

object Nvu_mean_main{
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(Nvu_mean(elem_nums=8))
  }
}
