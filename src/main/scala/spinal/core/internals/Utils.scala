package spinal.core.internals

import spinal.core._
import spinal.lib._

import scala.annotation.StaticAnnotation
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object share extends SpinalTag

object publicInput extends SpinalTag
object secretInput extends SpinalTag

/*
 * Custom annotation to indicate that a function contains signals that should be automatically named.
 */
class Name extends StaticAnnotation

object mod {
  def apply(x : UInt, m : Int) : UInt = {
    if (x.getBitsWidth >= log2Up(m)) {
      (x%m)(log2Up(m)-1 downto 0)
    } else {
      (x.resize(log2Up(m))%m)(log2Up(m)-1 downto 0)
    }
  }

  // Properly compute a mod q for scala integer types
  def apply[T](a: T, q: T)(implicit num: Integral[T]): T = {
    import num._
    if ((a % q) >= zero) a % q else (a % q) + q
  }

}

/**
 * Rotate a vector a by n positions to the left
 */
object rotateLeft {
  def apply[T <: Data](a : Vec[T], n: Int) : Vec[T] = {
    a.shuffle((0 until a.length).map(x => {
      // Make sure that the index is always positive
      if (x - n < 0) x - n + a.length else x - n
    }))
  }
}

/**
 * Rotate a vector a by n positions to the right
 */
object rotateRight {
  def apply[T <: Data](a : Vec[T], n: Int) : Vec[T] = {
    a.shuffle((0 until a.length).map(x => (x + n) % a.length))
  }
}

/**
 * Helper function which prints the given 128-bit AES state during simulation for debugging purposes.
 */
object reportAESState {
  def apply(title: String, state: Vec[Bits]): Unit = {
    report(title)
    for (i <- 0 until 4) {
      report(Seq(state(i), " ", state(4 + i), " ", state(8 + i), " ", state(12 + i)))
    }
    report("-------------------------------")
  }
}

/**
 * Helper function which prints the given AES key state during simulation for debugging purposes.
 */
object reportAESKeyState {
  def apply(title: String, state: Vec[Bits]): Unit = {
    report(title)
    report(state.length match {
      case 16 => Seq(state(0), state(1), state(2), state(3), " ", state(4), state(5), state(6), state(7), " ", state(8), state(9), state(10), state(11), " ", state(12), state(13), state(14), state(15))
      case 24 => Seq(state(0), state(1), state(2), state(3), " ", state(4), state(5), state(6), state(7), " ", state(8), state(9), state(10), state(11), " ", state(12), state(13), state(14), state(15), " ", state(16), state(17), state(18), state(19), " ", state(20), state(21), state(22), state(23))
      case 32 => Seq(state(0), state(1), state(2), state(3), " ", state(4), state(5), state(6), state(7), " ", state(8), state(9), state(10), state(11), " ", state(12), state(13), state(14), state(15), " ", state(16), state(17), state(18), state(19), " ", state(20), state(21), state(22), state(23), " ", state(24), state(25), state(26), state(27), " ", state(28), state(29), state(30), state(31))
    })
    report("-------------------------------")
  }
}

/**
 * Read a config from src/main/scala/Gadgets and update the areaConfig
 */
object ReadAreaConfig {
  def apply(file : String): Unit = {
    val wd = os.pwd / "src" / "main" / "scala" / "Gadgets"
    val jsonString = os.read(wd / file)
    val data = ujson.read(jsonString)
    val areaConfig = mutable.Map[String, mutable.Map[String, ArrayBuffer[Double]]]()

    val gadgets = data("Gadgets").arr
    gadgets.foreach(gadget => {
      val gadgetName = gadget("Name").str
      areaConfig += (gadgetName -> mutable.Map[String, ArrayBuffer[Double]]())
      for (gateType <- List("AND", "XOR", "NOT", "REG")) {
        val gate = gadget(gateType)
        areaConfig(gadgetName) += (gateType -> ArrayBuffer[Double]())
        for (i <- 0 to 10) {
          areaConfig(gadgetName)(gateType) += gate(i.toString).value.toString.toDouble
        }
      }
    })

    GlobalConfig.setAreaConfig(areaConfig)
  }
}

object ConvertStringToOpt {
  def apply(s : String): OptimizationTarget = {
    s match {
      case "Area" => Area
      case "ATP" => ATP
      case "ATRP" => ATRP
      case "Latency" => Latency
      case "Randomness" => Randomness
      case "Reload" => Reload
    }
  }
}

/**
  * Pretty print a BigInt as 3 digits followed by the exponent
  * Example: 1476 -> 147E+1
  *           978 -> 978E+0
  *         15023 -> 150E+2
  */
object PrettyPrint {
  def apply(x : BigInt): String = {
    if (x < 1000) s"${x}E+0"
    else {
      var exp = 0
      var xCopy = x
      while (xCopy >= 1000) {
        xCopy = xCopy / 10
        exp += 1
      }
      s"${xCopy}E+${exp}"
    }
  }
}