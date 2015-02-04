package com.scalax86.gen

import scala.xml._

trait OperandSize {
  def size: Int
  def name: String = null
  override def toString = {
    if (name == null) {
      if (size == 0)
        ""
      else
        size.toString
    } else
      name
  }
}

// sizes must always be ascending in size
sealed abstract class OperandType {
  def code: String
  val name: String
  def x87Only: Boolean

  override def toString = code
}

// These operandTypes depend on the @opsize field
sealed case class StandandOperandType(val code: String, val name: String, val promotedByRex: Boolean, val x87Only: Boolean) extends OperandType

// These operandTypes depend on the @opsize field
sealed case class CompositeOperandType(val code: String, val name: String, val types: Seq[String], val x87Only: Boolean) extends OperandType

// These operandTypes depend on the @opsize field
//sealed case class SizedOperandType(val code: String, val name: String, opsize: Int, val promotedByRex: Boolean, val x87Only: Boolean) extends OperandType

// These are fixed
sealed case class FixedOperandType(val code: String, val name: String, size: String, promotedByRex: Boolean, x87Only: Boolean) extends OperandType

object OperandType {
  def decodeOperandType: Map[String, OperandType] = {

    Map("a" -> CompositeOperandType("a", "Two 16 or 32 Byte Operands", Seq("w", "d"), false),
        "b" -> FixedOperandType("b", "Byte Operand", "8", false, false),
        "bcd" -> StandandOperandType("bcd", "Packed BCD", false, false),
        "bs" -> CompositeOperandType("bs", "Byte Sign Extended To Dst Op", Seq("b"), false),
        "bsq" -> StandandOperandType("bsq", "Byte Sign Extended To 64", false, false),
        "bss" -> CompositeOperandType("bss", "Byte Sign Extended To StackPtr", Seq("b"), false),
        "c" -> StandandOperandType("c","Byte Or Word", false, false),
        "d" -> FixedOperandType("d", "Doubleword", "32", false, false),
        "di" -> FixedOperandType("di", "Doubleword Int", "32", false, false),
        "dq" -> FixedOperandType("dq", "Double Quadword", "128", false, false),
        "dqp" -> CompositeOperandType("dqp", "Double Or Quadword", Seq("d", "q"), false),
        "dr" -> FixedOperandType("dr", "Double Real", "64fp", false, false),
        "ds" -> FixedOperandType("ds","Doubleword Sign Extended To 64", "32", false, false),
        "e" -> StandandOperandType("e", "X87 FPU Environment", false, false),
        "er" -> StandandOperandType("er", "Extended Real", false, false),
        "p" -> StandandOperandType("p", "Thirty Two Or 48 Bit Pointer", false, false),
        "pi" -> FixedOperandType("pi", "Quadword MMX", "64", false, false),
        "pd" -> FixedOperandType("pd", "Bit Packed 128 Double Precision Float", "16", false, false),
        "ps" -> FixedOperandType("ps", "Bit Packed 128 Single Precision Float", "16", false, false),
        "psq" -> FixedOperandType("psq", "Bit Packed 64 Single Precision Float", "8", false, false),
        "ptp" -> StandandOperandType("ptp", "Thirty Two Or 48 Or80BitPointer", true, false),
        "q" -> FixedOperandType("q", "Quadword Regardless", "64", false, false),
        "qi" -> StandandOperandType("qi", "Quadword Integer", false, false),
        "qp" -> FixedOperandType("qp", "Quadword Promoted", "64", true, false),
        "s" -> StandandOperandType("s", "Pseudo Descriptor", false, false),
        "sd" -> StandandOperandType("sd", "Scalar Packed Double Precision Float", false, false),
        "si" -> StandandOperandType("si", "Double Word Integer Register", false, false),
        "sr" -> FixedOperandType("sr", "Single Real", "32fp", false, true),
        "ss" -> StandandOperandType("ss", "Scalar Packed Single Precision Float", false, false),
        "st" -> StandandOperandType("st", "X87 FPU State", false, true),
        "stx" -> StandandOperandType("stx", "X87 FPU And SIMD State", false, true),
        "t" -> StandandOperandType("t", "Ten Byte Far Pointer", false, false),
        "v" -> CompositeOperandType("v", "Word Or Doubleword", Seq("w", "d"), false),
        "vds" -> CompositeOperandType("vds", "Word Or Doubleword or Doubleword Extended To 64", Seq("w", "d", "ds"), false), // TODO: Also include ds
        "vq" -> CompositeOperandType("vq", "Quadword Or Word", Seq("q", "w"), false),
        "vqp" -> CompositeOperandType("vqp", "Word Or Doubleword Or Quadword", Seq("w", "d", "qp"), false),
        "vs" -> CompositeOperandType("vs", "Word Or Doubleword Extended To Stack", Seq("w", "d"), false),
        "w" -> FixedOperandType("w", "Word", "16", false, false),
        "wi" -> FixedOperandType("wi", "Word Integer", "16", false, false),
        "va" -> StandandOperandType("va", "Word Or Doubleword Based On Address Size", false, false),
        "dqa" -> StandandOperandType("dqa", "Doubleword Or Quadword Based On Address Size", false, false),
        "wa" -> StandandOperandType("wa", "Word Based On Address Size", false, false),
        "wo" -> StandandOperandType("wo", "Word Based On Operand Size", false, false),
        "ws" -> StandandOperandType("ws", "Word Based On Stack Size", false, false),
        "da" -> StandandOperandType("da", "Doubleword Based On Address Size", false, false),
        "do" -> StandandOperandType("do", "Doubleword Based On Operand Size", false, false),
        "qa" -> StandandOperandType("qa", "Quadword Based On Address Size", false, false),
        "qs" -> StandandOperandType("qs", "Quadword Based On Operand Size", false, false))
  }
}