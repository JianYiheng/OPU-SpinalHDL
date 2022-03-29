package nvu.common

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import spinal.core.SpinalConfig
import spinal.core.sim.SimConfig

import scala.sys.process._
import scala.util.Random
import scala.collection.mutable.Stack

object nvuSimConfig {
  def apply() = SimConfig.withFstWave.withConfig(SpinalConfig(targetDirectory = "rtl")).workspacePath("waves")
}

//Run this scala test to generate and check that your RTL work correctly
class Nvu_common_tb extends FunSuite {
  test("nvu_max_testbench") {
    nvuSimConfig().compile(new Nvu_max_tb(elem_nums=8)).doSim{ dut =>
      dut.init()
      val src: Stack[Array[Int]] = dut.source()
      dut.driveAndCheck(src)
    }
  }

  test("nvu_lut_testbench") {
    nvuSimConfig().compile(new Nvu_lut_tb()).doSim{ dut =>
      dut.init()
      val src: Stack[Double] = dut.source()
      dut.driveAndCheck(src)
    }
  }
}
