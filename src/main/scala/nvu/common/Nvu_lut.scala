package nvu.common

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.util.Random
import scala.math
import scala.collection.mutable.Stack 

import spinal.lib.dsptool.FixData

case class Nvu_lut() extends Component {
  val io = new Bundle {
    val x_vld = in Bool()
    val y_vld = out Bool()

    val lut_x = in Vec(SFix(Nvu_params.LUT_X_INTG exp, -Nvu_params.LUT_X_FRAC exp), Nvu_params.LUT_X_LEN)
    val lut_y = in Vec(SFix(Nvu_params.LUT_Y_INTG exp, -Nvu_params.LUT_Y_FRAC exp), Nvu_params.LUT_Y_LEN)
    val lut_k = in Vec(SFix(Nvu_params.LUT_K_INTG exp, -Nvu_params.LUT_K_FRAC exp), Nvu_params.LUT_K_LEN)

    val x_i   = in SFix(Nvu_params.LUT_IN_INTG exp, -Nvu_params.LUT_IN_FRAC exp)
    val y_i   = out SFix(Nvu_params.LUT_OUT_INTG exp, -Nvu_params.LUT_OUT_FRAC exp)
  }

  noIoPrefix()

  val lut_pos_ary = Reg(Bits(Nvu_params.LUT_X_LEN bits)) init(0)
  val lut_pos     = CountOne(lut_pos_ary)-1

  val lut_x_r     = Reg(SFix(Nvu_params.LUT_X_INTG exp, -Nvu_params.LUT_X_FRAC exp)) init(0)
  val lut_y_r     = Reg(SFix(Nvu_params.LUT_Y_INTG exp, -Nvu_params.LUT_Y_FRAC exp)) init(0)
  val lut_y_r_1   = Reg(SFix(Nvu_params.LUT_Y_INTG exp, -Nvu_params.LUT_Y_FRAC exp)) init(0)
  val lut_k_r     = Reg(SFix(Nvu_params.LUT_K_INTG exp, -Nvu_params.LUT_K_FRAC exp)) init(0)

  val delta       = SFix(Nvu_params.LUT_DELTA_INTG exp, -Nvu_params.LUT_DELTA_FRAC exp)
  val delta_one   = SFix(Nvu_params.LUT_DELTA_INTG+1 exp, -Nvu_params.LUT_DELTA_FRAC exp)
  val delta_a     = Reg(SFix(Nvu_params.LUT_DELTA_INTG exp, -Nvu_params.LUT_DELTA_FRAC exp)) init(0)
  val delta_b     = Reg(SFix(Nvu_params.LUT_DELTA_INTG exp, -Nvu_params.LUT_DELTA_FRAC exp)) init(0)

  when (io.x_vld) {
    for (i <- 0 until Nvu_params.LUT_X_LEN) {
      when (io.x_i > io.lut_x(i)) {
        lut_pos_ary(i)  := True
      }.otherwise {
        lut_pos_ary(i)  := False
      }
    }
  }

  when (Delay(io.x_vld, 1, init=False)) {
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

  when (Delay(io.x_vld, 2, init=False)) {
    delta_a := delta
    delta_b := (delta_one - delta).truncated
  }

  io.y_i := Vec(delta_b, delta_a).zip(Vec(Delay(lut_y_r, 1), Delay(lut_y_r_1, 1))).map{case(a, b)=>RegNext(a * b)}.reduceBalancedTree(_+_,(s,l)=>RegNext(s)).truncated

  io.y_vld := Delay(io.x_vld, 5, init=False)

}

class Nvu_lut_tb extends Nvu_lut {
  def init () {
    clockDomain.forkStimulus(10)
    io.x_i.raw   #= 0
    io.x_vld     #= false

    val lut_x = List(-6, -5, -4, -3, -2, -1, 0).map(FixData(_, SQ(Nvu_params.DATA_WIDTH, Nvu_params.LUT_X_FRAC)).asLong)
    val lut_y = List(0.0024787521766663585, 0.006737946999085467, 0.01831563888873418, 0.049787068367863944, 0.1353352832366127, 0.36787944117144233, 1).map(FixData(_, SQ(Nvu_params.DATA_WIDTH, Nvu_params.LUT_Y_FRAC)).asLong)
    val lut_k = List(1.0, 1.0, 1.0, 1.0, 1.0, 1.0).map(FixData(_, SQ(Nvu_params.DATA_WIDTH, Nvu_params.LUT_K_FRAC)).asLong)

    for (i <- 0 until Nvu_params.LUT_X_LEN) {
      io.lut_x(i).raw #= lut_x(i)
      io.lut_k(i).raw #= lut_k(i)
    }

    for (i <- 0 until Nvu_params.LUT_Y_LEN) {
      io.lut_y(i).raw #= lut_y(i)
    }
    sleep(100)
  }

  def source (): Stack[Double] = {
    var src: Stack[Double] = Stack()
    val src_max = -0.2
    val src_min = -6
    for (_ <- 0 until 100) {
      src.push(Random.nextDouble()*(src_max-src_min)+src_min)
    }

    return src
  }

  def driveAndCheck (src: Stack[Double]) {
    val src_drv, src_chk = src.clone
    val drive_task = fork {
      while (src_drv.length>0) {
        if (Random.nextDouble() < 0.5) {
          io.x_i.raw #= 0
          io.x_vld   #= false
          clockDomain.waitSampling()
        } else {
          val data_src: Double = src_drv.pop()
          io.x_i.raw #= (data_src*math.pow(2, Nvu_params.LUT_X_FRAC)).toInt
          io.x_vld   #= true
          clockDomain.waitSampling()
        }
      }
    }

    val check_task = fork {
      while (src_chk.length>0) {
        if (io.y_vld.toBoolean) {
          val data_src = src_chk.pop()
          val data_ref = math.exp(data_src)
          // println(s"data_src: ${data_src}, data_ref: ${data_ref}, y_i: ${io.y_i.raw.toInt/math.pow(2, Nvu_params.LUT_OUT_FRAC)}")
          assert ((data_ref - io.y_i.raw.toInt/math.pow(2, Nvu_params.LUT_OUT_FRAC)).abs / data_ref.abs < 0.5, s"data_src: ${data_src}, data_ref: ${data_ref}, y_i: ${io.y_i.raw.toInt/math.pow(2, Nvu_params.LUT_OUT_FRAC)}")
        }
        clockDomain.waitSampling()
      }
    }

    drive_task.join()
    check_task.join()
  }
}

object Nvu_lut_main {
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(Nvu_lut())
  }
}

