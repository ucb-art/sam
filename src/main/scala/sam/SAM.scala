// See LICENSE for license details.

// Author: Stevo Bailey (stevo.bailey@berkeley.edu)

package sam

import chisel3.util._
import chisel3._
import dsptools.junctions._
import scala.math._
import dsptools.numbers.{Real, DspComplex}
import dsptools.numbers.implicits._
import dsptools._
import cde.Parameters
import junctions._
import util._

class SAMIO[T<:Data:Real]()(implicit val p: Parameters) extends Bundle with HasSAMGenParameters[T] {
  val config = p(SAMKey)(p)

  val in = Input(ValidWithSync(Vec(lanesIn, genIn())))
  val out = Flipped(new NastiIO)
}

class SAM[T<:Data:Real]()(implicit val p: Parameters) extends Module with HasSAMGenParameters[T] {
  val io = IO(new SAMIO[T])
  val config = p(SAMKey)(p)

  // memories
  // TODO: ensure that the master never tries to read beyond the depth of the SeqMem
  val mem = SeqMem(config.subpackets*config.bufferDepth, UInt(width = w))

  // TODO: AXI4Stream side

  // TODO: ensure that the reading never happens beyond where the writing has occurred

  // AXI4 side
  val rIdle :: rWait :: rReadFirst :: rSend :: Nil = Enum(Bits(), 4)
  val state = Reg(UInt(3.W), init=rIdle)
  val rState = Reg(init = rIdle)
  val rAddr = Reg(UInt(width = nastiXAddrBits - log2Ceil(w/8)))
  val rLen = Reg(UInt(width = nastiXLenBits - log2Ceil(w/nastiXDataBits)))
  val rawData = mem.read(rAddr) // this will read every cycle; how do we make it single-ported?
  val rData = Reg(UInt(width = w))
  val rCount =
    if (w == nastiXDataBits) Reg(UInt(width = 1))
    else Reg(UInt(width = log2Ceil(w/nastiXDataBits)))
  val rId = Reg(io.axi.ar.bits.id.cloneType)

  // state must be Idle here, since fire happens when ar.ready is high
  // grab the address information, then wait for data
  when (io.axi.ar.fire()) {
    rAddr := io.axi.ar.bits.addr >> UInt(log2Ceil(w/8))
    rLen  := io.axi.ar.bits.len  >> UInt(log2Ceil(w/nastiXDataBits))
    rState := rWait
  }

  // delay state by a cycle to align with SeqMem, I think
  when (state === rWait) { rState := rReadFirst }
  when (state === rReadFirst) {
    rData := rawData
    rAddr := rAddr + UInt(1)
    rCount := UInt(w/nastiXDataBits-1)
    rId := io.axi.ar.bits.id // seems unnecessary, since rId is always this
    rState := rSend
  }

  // wait for ready from master when in Send state
  when (io.axi.r.fire()) {
    when (rCount === UInt(0)) {
      when (rLen === UInt(0)) {
        rState := rIdle
      } .otherwise {
        rData := rawData
        rAddr := rAddr + UInt(1)
        rLen := rLen - UInt(1)
        rCount := UInt(w/nastiXDataBits-1)
      }
    } .otherwise {
      rData := rData >> UInt(nastiXDataBits)
      rCount := rCount - UInt(1)
    }
  }

  io.axi.ar.ready := (rState === rIdle)
  io.axi.r.valid := (rState === rSend)
  io.axi.r.bits := NastiReadDataChannel(
    id = rId,
    data = rData(nastiXDataBits - 1, 0),
    last = rLen === UInt(0) && rCount === UInt(0))

  // no write capabilities yet
  io.axi.aw.ready := Bool(false)
  io.axi.w.ready := Bool(false)
  io.axi.b.valid := Bool(false)

  // assert(w % nastiXDataBits === 0)

  assert(!io.axi.ar.valid ||
    (io.axi.ar.bits.addr(log2Ceil(w/8)-1, 0) === UInt(0) &&
     io.axi.ar.bits.len(log2Ceil(w/nastiXDataBits)-1, 0).andR &&
     io.axi.ar.bits.size === UInt(log2Up(nastiXDataBits/8))),
   "Invalid read request")

  // required by AXI4 spec
  when (reset) {
    io.axi.r.valid := Bool(false)
  }

}

class SAMWrapper[T<:Data:Real]()(implicit p: Parameters) extends GenDspBlock[T, T]()(p) with HasSAMGenParameters[T] {

  // SCR 
  val baseAddr = BigInt(0)
  val sam = Module(new SAM[T])
  val config = p(FIRKey)(p)

  (0 until config.numberOfTaps).map( i =>
    addControl(s"firCoeff$i", 0.U)
  )
  addStatus("firStatus")

  fir.io.in <> unpackInput(lanesIn, genIn())
  val taps = Wire(Vec(config.numberOfTaps, genTap.getOrElse(genIn())))
  val w = taps.zipWithIndex.map{case (x, i) => x.fromBits(control(s"firCoeff$i"))}
  fir.io.taps := w

  unpackOutput(lanesOut, genOut()) <> fir.io.out
  status("firStatus") := fir.io.out.sync
}
