package draft

import spinal.core._
import scala.util.Random
import scala.math

import spinal.core.sim._
import spinal.core.SpinalConfig
import spinal.core.sim.SimConfig

import scala.collection.mutable.Stack

case class Counter(width: Int) extends Component {
  val io = new Bundle {
    val x_i = in UInt(width bits)
    val y_i = out UInt(width bits)
  }

  io.y_i := RegNext(io.x_i)

}

class CounterTb(width: Int) extends Counter(width) {
  def init () {
    clockDomain.forkStimulus(10)
    io.x_i #= 0
    clockDomain.waitSampling()
  }

  def source (): Stack[Int] = {
    var data: Stack[Int] = Stack()
    for (_ <- 0 until 100) {
      data.push(Random.nextInt(math.pow(2, width).toInt))
    }

    return data
  }

  def driver (src: Stack[Int]) {
    while (src.length>0) {
      val data = src.pop()
      io.x_i #= data
      clockDomain.waitSampling()
    }
  }

  def checker (src: Stack[Int]) {
    clockDomain.waitSampling()
    while (src.length>0) {
      val data = src.pop()
      clockDomain.waitSampling()
      assert(io.y_i == data)
    }
  }
}

//Run this main to generate the RTL
object CounterMain{
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(Counter(width = 4))
  }
}
