package com.scalax86.gen

sealed abstract class AddressingMethod(val abbreviation: String, val hasRMByte: Boolean) {
  override def toString = abbreviation
}
object DirectAddress extends AddressingMethod("ptr", false)
object MemoryAddressedbyAX extends AddressingMethod("m", false)
object MemoryAddressedbyAXPlusAL extends AddressingMethod("m", false)
object MemoryAddressedbyDS extends AddressingMethod("m", false)
object RegFieldSelectsControlRegister extends AddressingMethod("CRn", true)
object RegFieldSelectsDebugRegister extends AddressingMethod("DRn", true)
object ModRMByteRegisterOrMemory extends AddressingMethod("rm", true)
object ModRMByteX87StackOrMemory extends AddressingMethod("m", true)
object ModRMByteX87StackRegister extends AddressingMethod("STi", true)
object FlagsRegister extends AddressingMethod("-", false)
object RegFieldRegister extends AddressingMethod("r", true)
object RMFieldRegisterAlways extends AddressingMethod("r", true)
object ImmediateData extends AddressingMethod("imm", false)
object RelativeOffset extends AddressingMethod("rel", false)
case object ModRMByteMemoryOnly extends AddressingMethod("m", true)
object RMFieldMMXRegister extends AddressingMethod("mm", true)
object NoModRMByteOrSIBWithOffset extends AddressingMethod("moffs", false)
object RegFieldMMXRegister extends AddressingMethod("mm", true)
object ModRMByteMMXRegOrMemory extends AddressingMethod("mm/m64", true)
object ModFieldRegister extends AddressingMethod("r", true)
object RegFieldSegmentRegister extends AddressingMethod("Sreg", true)
object StackOperand extends AddressingMethod("-", false)
object RegFieldTestRegister extends AddressingMethod("TRn", true)
object RMField128XMM extends AddressingMethod("xmm", true)
object RegField128XMM extends AddressingMethod("xmm", true)
object ModRMByte128XXMOrMemory extends AddressingMethod("xmm/m", true)
object MemoryAddressedbySI extends AddressingMethod("m", false)
object MemoryAddressedbyDI extends AddressingMethod("m", false)
case object OpcodeSelectsRegister extends AddressingMethod("r", false)
object S2 extends AddressingMethod("S2", false)
object S30 extends AddressingMethod("S30", false)
object S33 extends AddressingMethod("S33", false)

object AddressingMethod {
  def decodeAddressingMethod(a: String): AddressingMethod = {
    a match {
      case "A"   => DirectAddress
      case "BA"  => MemoryAddressedbyAX
      case "BB"  => MemoryAddressedbyAXPlusAL
      case "BD"  => MemoryAddressedbyDS
      case "C"   => RegFieldSelectsControlRegister
      case "D"   => RegFieldSelectsDebugRegister
      case "E"   => ModRMByteRegisterOrMemory
      case "ES"  => ModRMByteX87StackOrMemory
      case "EST" => ModRMByteX87StackRegister
      case "F"   => FlagsRegister
      case "G"   => RegFieldRegister
      case "H"   => RMFieldRegisterAlways
      case "I"   => ImmediateData
      case "J"   => RelativeOffset
      case "M"   => ModRMByteMemoryOnly
      case "N"   => RMFieldMMXRegister
      case "O"   => NoModRMByteOrSIBWithOffset
      case "P"   => RegFieldMMXRegister
      case "Q"   => ModRMByteMMXRegOrMemory
      case "R"   => ModFieldRegister
      case "S"   => RegFieldSegmentRegister
      case "SC"  => StackOperand
      case "T"   => RegFieldTestRegister
      case "U"   => RMField128XMM
      case "V"   => RegField128XMM
      case "W"   => ModRMByte128XXMOrMemory
      case "X"   => MemoryAddressedbySI
      case "Y"   => MemoryAddressedbyDI
      case "Z"   => OpcodeSelectsRegister
      case "S2"   => S2
      case "S30"   => S30
      case "S33"   => S33
    }
  }
}