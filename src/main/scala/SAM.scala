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
class SAMIO[T<:Data:Real](genIn: => T, val config: SAMConfig = SAMConfig()) extends Bundle {

  val in = Input(ValidWithSync(genIn))
  val axi = Output(new NastiIO)
}

// stream to axi4 memory
class SAM[T<:Data:Real](genIn: => T, val config: SAMConfig = SAMConfig()) extends Module {

  val io = IO(new SAMIO(genIn, config))

  // state machine stuff
  val sIdle :: sTriggered :: sWriting :: sReading :: Nil = Enum(UInt(), 4)
  val stateWidth = log2Up(4)
  val state = Reg(UInt(width=stateWidth), init=sIdle)

  // memories
  val write_indices = log2Up(config.subpackets*config.bufferDepth)
  val write_index = Counter(state === sWriting && io.in.valid, write_indices)._1
  val mem = SeqMem(config.subpackets*config.bufferDepth, genIn)
  
  // axi reading stuff
  // TODO: what are these variables? 
  val ar = Queue(io.axi.ar, 1)
  val read_index = ar.bits.addr(4, 3)
  io.axi.r.valid := ar.valid
  ar.ready := io.axi.r.ready

  // le state machine
  switch(state) {
    is sIdle {
     
    }
    is sTriggered {
      when (io.in.sync && io.in.valid) { state := sWriting }
    }
    is sWriting {
      when (io.in.valid) {
        mem(write_index) := io.in.bits
        when (write_index === UInt(write_indices-1)) {
          state := sIdle
        }
      }
    }
    is sReading {
    }
  }
}

