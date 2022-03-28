package draft

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import spinal.core.SpinalConfig
import spinal.core.sim.SimConfig

import scala.sys.process._
import scala.util.Random
import scala.collection.mutable.Stack

//Run this scala test to generate and check that your RTL work correctly
class CounterTester extends FunSuite {
  var compiled: SimCompiled[CounterTb] = null

  test("testbench") {
    SimConfig.withWave.withConfig(SpinalConfig(targetDirectory = "rtl")).workspacePath("waves").compile(new CounterTb(width=4)).doSim{ dut =>
      dut.init()
      val src: Stack[Int] = dut.source()
      dut.driver(src)
      dut.checker(src)
    }
  }
}
