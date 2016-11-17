// See LICENSE for license details.

// Author: Stevo Bailey (stevo.bailey@berkeley.edu)

package sam

import chisel3.util._
import chisel3._
import dsptools.numbers.Real
import dsptools.numbers.implicits._
import dsptools.junctions._

case class SAMConfig(subpackets: Int, bufferDepth: Int = 2) {
  
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

  // state machine stuff
  val sIdle :: sTriggered :: sWriting :: sReading :: Nil = Enum(UInt(), 4)
  val state = Reg(init = sIdle)

  // memories
  val write_indices = log2Ceil(config.subpackets*config.bufferDepth)
  val write_index = Counter(state === sWriting && io.in.valid, write_indices)._1
  val mem = SeqMem(config.subpackets*config.bufferDepth, UInt(width = w))
  
  // le state machine
  switch(state) {
    is (sIdle) {
     
    }
    is (sTriggered) {
      when (io.in.sync && io.in.valid) { state := sWriting }
    }
    is (sWriting) {
      when (io.in.valid) {
        mem(write_index) := io.in.bits
        when (write_index === UInt(write_indices-1)) {
          state := sIdle
        }
      }
    }
    is (sReading) {
    }
  }

  val rIdle :: rWait :: rReadFirst :: rSend :: Nil = Enum(Bits(), 3)
  val rState = Reg(init = rIdle)
  val rAddr = Reg(UInt(width = nastiXAddrBits - log2Ceil(w/8)))
  val rLen = Reg(UInt(width = nastiXLenBits - log2Ceil(w/nastiXDataBits)))
  val rawData = mem.read(rAddr)
  val rData = Reg(UInt(width = w))
  val rCount =
    if (w == nastiXDataBits) Reg(UInt(width = 1))
    else Reg(UInt(width = log2Ceil(w/nastiXDataBits)))
  val rId = Reg(io.ar.bits.id)

  when (io.ar.fire()) {
    rAddr := io.ar.bits.addr >> UInt(log2Ceil(w/8))
    rLen  := io.ar.bits.len  >> UInt(log2Ceil(w/nastiXDataBits))
    rState := rWait
  }

  when (state === rWait) { rState := rRead }
  when (state === rReadFirst) {
    rData := rawData
    rAddr := rAddr + UInt(1)
    rCount := UInt(w/nastiXDataBits-1)
    rId := io.ar.bits.id
    rState := rSend
  }
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

  io.aw.ready := Bool(false)
  io.w.ready := Bool(false)
  io.b.valid := Bool(false)

  require(w % nastiXDataBits === 0)

  assert(!io.ar.valid ||
    (io.ar.bits.addr(log2Ceil(w/8)-1, 0) === UInt(0) &&
     io.ar.bits.len(log2Ceil(w/nastiXDataBits)-1, 0).andR &&
     io.ar.bits.size === UInt(log2Up(nastiXDataBits/8))),
   "Invalid read request")
}
