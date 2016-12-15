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
import _root_.junctions._
import util._

class SAMIO[T<:Data:Real]()(implicit val p: Parameters) extends Bundle with HasSAMGenParameters[T] {
  val in = Input(ValidWithSync(genIn()))
  val out = new NastiIO().flip
}

class SAM[T<:Data:Real]()(implicit p: Parameters) extends NastiModule()(p) with HasSAMGenParameters[T] {
  val io = IO(new SAMIO[T])
  val w = p(DspBlockKey).inputWidth

  // memories
  // TODO: ensure that the master never tries to read beyond the depth of the SeqMem
  val mem = SeqMem(samConfig.subpackets*samConfig.bufferDepth, UInt(width = w))

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
  val rId = Reg(io.out.ar.bits.id.cloneType)

  // state must be Idle here, since fire happens when ar.ready is high
  // grab the address information, then wait for data
  when (io.out.ar.fire()) {
    rAddr := io.out.ar.bits.addr >> UInt(log2Ceil(w/8))
    rLen  := io.out.ar.bits.len  >> UInt(log2Ceil(w/nastiXDataBits))
    rState := rWait
  }

  // delay state by a cycle to align with SeqMem, I think
  when (state === rWait) { rState := rReadFirst }
  when (state === rReadFirst) {
    rData := rawData
    rAddr := rAddr + UInt(1)
    rCount := UInt(w/nastiXDataBits-1)
    rId := io.out.ar.bits.id // seems unnecessary, since rId is always this
    rState := rSend
  }

  // wait for ready from master when in Send state
  when (io.out.r.fire()) {
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

  io.out.ar.ready := (rState === rIdle)
  io.out.r.valid := (rState === rSend)
  io.out.r.bits := NastiReadDataChannel(
    id = rId,
    data = rData(nastiXDataBits - 1, 0),
    last = rLen === UInt(0) && rCount === UInt(0))

  // no write capabilities yet
  io.out.aw.ready := Bool(false)
  io.out.w.ready := Bool(false)
  io.out.b.valid := Bool(false)

  // assert(w % nastiXDataBits === 0)

  assert(!io.out.ar.valid ||
    (io.out.ar.bits.addr(log2Ceil(w/8)-1, 0) === UInt(0) &&
     io.out.ar.bits.len(log2Ceil(w/nastiXDataBits)-1, 0).andR &&
     io.out.ar.bits.size === UInt(log2Up(nastiXDataBits/8))),
   "Invalid read request")

  // required by AXI4 spec
  when (reset) {
    io.out.r.valid := Bool(false)
  }

}

class SAMWrapper[T<:Data:Real]()(implicit p: Parameters) extends GenDspBlock[T, T]()(p) with HasSAMGenParameters[T] {

  // SCR 
  val baseAddr = BigInt(0)
  val sam = Module(new SAM[T])

  addControl("samControl", 0.U)
  addStatus("samStatus")
  sam.io.in.sync := control("samControl")(0)

  sam.io.in <> io.in
  status("samStatus") := sam.io.out.r.valid
}
