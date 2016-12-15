package sam

import breeze.math.{Complex}
import breeze.signal.{fourierTr}
import breeze.linalg._
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
import sam.Generator.params

import dsptools._
import scala.collection.mutable.Map

trait HasIPXACTParameters {
  def getIPXACTParameters: Map[String, String]
}

// create a new DSP Configuration
class DspConfig extends Config(
  (pname, site, here) => pname match {
    case BuildDSP => { (q: Parameters) => {
      implicit val p = q
      Module(new SAMWrapper[DspReal])
    }}
    case SAMKey => SAMConfig(10, 10)
    case NastiKey => NastiParameters(64, 32, 1)
    case PAddrBits => 32
    case CacheBlockOffsetBits => 6
    case AmoAluOperandBits => 64
    case TLId => "SAM"
    case TLKey("SAM") =>
      TileLinkParameters(
        coherencePolicy = new MEICoherence(
          new NullRepresentation(2)),
        nManagers = 1,
        nCachingClients = 0,
        nCachelessClients = 1,
        maxClientXacts = 4,
        maxClientsPerPort = 1,
        maxManagerXacts = 1,
        dataBeats = 8,
        dataBits = 64 * 8)
    case DspBlockKey => DspBlockParameters(1024, 1024)
    case GenKey => new GenParameters {
      def genIn [T <: Data] = UInt(1024.W).asInstanceOf[T]
      // not used
      val lanesIn = 1
    }
    case _ => throw new CDEMatchError
  }) with HasIPXACTParameters {
  def getIPXACTParameters: Map[String, String] = {
    // Get unadulterated, top level parameters.
    val parameterList = List[Field[_]](TLId, PAddrBits)
    val parameterMap = parameterList.foldLeft(Map[String, String]()) { (m, s) => m(s.toString) = params(s).toString; m }

    // Conjure up some IPXACT synthsized parameters.
    parameterMap ++= List(("InputLanes", params(GenKey).lanesIn.toString), ("OutputLanes", params(GenKey).lanesOut.toString))
    parameterMap ++= List(("InputTotalBits", params(DspBlockKey).inputWidth.toString))

    parameterMap
  }
}

case object SAMKey extends Field[SAMConfig]

trait HasSAMGenParameters[T <: Data] extends HasGenParameters[T, T] {
  val p: Parameters
  val samConfig = p(SAMKey)
}

case class SAMConfig(
  val subpackets: Int,
  val bufferDepth: Int
)
