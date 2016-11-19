// See LICENSE for license details.

// Author: Stevo Bailey (stevo.bailey@berkeley.edu)

package sam

import chisel3.util._
import chisel3._
import dsptools.numbers.Real
import dsptools.numbers.implicits._
import dsptools.junctions._

// requires rocketchip
import junctions._
import util._

case class SAMConfig(subpackets: Int = 2, bufferDepth: Int = 2) {
  
}

// stream to axi4 memory io
class SAMIO[T<:Data:Real](genIn: => T, val config: SAMConfig = SAMConfig())
                         (implicit p: Parameters) extends ParameterizedBundle()(p) {

  val in = Input(ValidWithSync(genIn))
  val axi = Output(new NastiIO)
}

// stream to axi4 memory
class SAM[T<:Data:Real](w: Int, val config: SAMConfig = SAMConfig())
                       (implicit p: Parameters) extends NastiModule()(p) {

  val io = IO(new SAMIO(UInt(width = w), config))

  // memories
  // TODO: ensure that the master never tries to read beyond the depth of the SeqMem
  val mem = SeqMem(config.subpackets*config.bufferDepth, UInt(width = w))

  // TODO: AXI4Stream side

  // TODO: ensure that the reading never happens beyond where the writing has occurred

  // AXI4 side
  val rIdle :: rWait :: rReadFirst :: rSend :: Nil = Enum(Bits(), 3)
  val rState = Reg(init = rIdle)
  val rAddr = Reg(UInt(width = nastiXAddrBits - log2Ceil(w/8)))
  val rLen = Reg(UInt(width = nastiXLenBits - log2Ceil(w/nastiXDataBits)))
  val rawData = mem.read(rAddr) // this will read every cycle; how do we make it single-ported?
  val rData = Reg(UInt(width = w))
  val rCount =
    if (w == nastiXDataBits) Reg(UInt(width = 1))
    else Reg(UInt(width = log2Ceil(w/nastiXDataBits)))
  val rId = Reg(io.ar.bits.id)

  // state must be Idle here, since fire happens when ar.ready is high
  // grab the address information, then wait for data
  when (io.ar.fire()) {
    rAddr := io.ar.bits.addr >> UInt(log2Ceil(w/8))
    rLen  := io.ar.bits.len  >> UInt(log2Ceil(w/nastiXDataBits))
    rState := rWait
  }

  // delay state by a cycle to align with SeqMem, I think
  when (state === rWait) { rState := rReadFirst }
  when (state === rReadFirst) {
    rData := rawData
    rAddr := rAddr + UInt(1)
    rCount := UInt(w/nastiXDataBits-1)
    rId := io.ar.bits.id // seems unnecessary, since rId is always this
    rState := rSend
  }

  // wait for ready from master when in Send state
  when (io.r.fire()) {
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

  io.ar.ready := (rState === rIdle)
  io.r.valid := (rState === rSend)
  io.r.bits := NastiReadDataChannel(
    id = rId,
    data = rData(nastiXDataBits - 1, 0),
    last = rLen === UInt(0) && rCount === UInt(0))

  // no write capabilities yet
  io.aw.ready := Bool(false)
  io.w.ready := Bool(false)
  io.b.valid := Bool(false)

  require(w % nastiXDataBits === 0)

  assert(!io.ar.valid ||
    (io.ar.bits.addr(log2Ceil(w/8)-1, 0) === UInt(0) &&
     io.ar.bits.len(log2Ceil(w/nastiXDataBits)-1, 0).andR &&
     io.ar.bits.size === UInt(log2Up(nastiXDataBits/8))),
   "Invalid read request")

  // required by AXI4 spec
  when (reset) {
    io.r.valid := Bool(false)
  }
}
