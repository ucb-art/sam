// See LICENSE for license details.
package sam

import dsptools.numbers.implicits._
import dsptools.Utilities._
import dsptools.{DspContext, Grow}
import spire.algebra.{Field, Ring}
import breeze.math.{Complex}
import breeze.linalg._
import breeze.signal._
import breeze.signal.support._
import breeze.signal.support.CanFilter._
import chisel3._
import chisel3.util._
import chisel3.iotesters._
import firrtl_interpreter.InterpreterOptions
import dsptools.numbers.{DspReal, SIntOrder, SIntRing}
import dsptools.{DspContext, DspTester, Grow}
import org.scalatest.{FlatSpec, Matchers}
import dsptools.numbers.implicits._
import dsptools.numbers.{DspComplex, Real}
import scala.util.Random
import scala.math._
import org.scalatest.Tag

import cde._
import junctions._
import uncore.tilelink._
import uncore.coherence._

import dsptools._

object LocalTest extends Tag("edu.berkeley.tags.LocalTest")

class SAMWrapperTester(c: SAMWrapper)(implicit p: Parameters) extends DspBlockTester(c) {
  val config = p(SAMKey)
  val gk = p(GenKey)

  // setup input streaming data
  def streamIn = Seq.fill(100)(BigInt(100))
  
  // pause stream while setting up SCR
  pauseStream

  val scr = testchipip.SCRAddressMap("SAMWrapper").get
  axiWrite(scr("samWStartAddr").toInt, 2)
  axiWrite(scr("samWTargetCount").toInt, 2)
  var s = axiRead(scr("samWState").toInt)
  println(s"State = $s")
  axiWrite(scr("samWTrig").toInt, 1)
  playStream
  step(1)
  pauseStream
  s = axiRead(scr("samWState").toInt)
  println(s"State = $s")
  playStream
  poke(c.io.in.sync, 1)
  step(1)
  poke(c.io.in.sync, 0)
  step(1)
  pauseStream
  s = axiRead(scr("samWState").toInt)
  println(s"State = $s")
}

class SAMWrapperSpec extends FlatSpec with Matchers {
  behavior of "SAMWrapper"
  val manager = new TesterOptionsManager {
    testerOptions = TesterOptions(backendName = "firrtl", testerSeed = 7L)
    interpreterOptions = InterpreterOptions(setVerbose = false, writeVCD = true)
  }

  it should "work with DspBlockTester" in {
    implicit val p: Parameters = Parameters.root(new DspConfig().toInstance)
    val dut = () => new SAMWrapper()
    chisel3.iotesters.Driver.execute(dut, manager) { c => new SAMWrapperTester(c) } should be (true)
  }
}
