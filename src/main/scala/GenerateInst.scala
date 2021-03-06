package com.scalax86.gen

import java.io._
import scala.xml._
import java.io.PrintWriter
import scala.collection.mutable.LinkedHashSet
import scala.collection.immutable.SortedMap
import scala.collection.mutable.ListBuffer

object GenerateInst {

  trait InstructionInstance {
    protected def getClassHeader(name: String): String
    protected def hasImplicitOperand: Boolean
    protected def getFormat: String
    val mnemonic: String
    def getSize: Int
    def opcode: Int
    def numOpcodeBytes: Int
    def entry: x86Entry
    def getOperand: Option[OperandInstance]
    def description = entry.brief
    
    override def equals(x: Object) = {
      if (x.isInstanceOf[InstructionInstance]) { 
        val otherInstance = x.asInstanceOf[InstructionInstance]
        //opcode == otherInstance.opcode && getOperand == otherInstance.getOperand
        generateClass("")(0) == otherInstance.generateClass("")(0)
      } else {
        false
      }
    }
    
    override def hashCode(): Int = {
      generateClass("")(0).hashCode
    }

    def generateClass(name: String): Seq[String] = {

      val header = getClassHeader(name)
      
      val rawOpcodeString = opcode.toHexString.toUpperCase.reverse.padTo(2, '0').reverse
      
      val simpleOpcodeString = if (numOpcodeBytes == 1) {
        ": OneOpcode = 0x" + rawOpcodeString
      } else if (numOpcodeBytes == 2) {
        ": TwoOpcodes = (0x0F, 0x" + rawOpcodeString + ")"
      }

      def getOpcodeString = {
        if (entry.hasRegisterInModRM) {
          "val opcode" + simpleOpcodeString + " /r"
        } else if (entry.opcodeEx.isDefined) {
          "val opcode" + simpleOpcodeString + " /+ " + entry.opcodeEx.get
        } else {
          "val opcode" + simpleOpcodeString
        }
      }

      val opcodeString: Seq[String] = getOperand.map { op =>
        op.addressingMethod match {
          case Some(OpcodeSelectsRegister) =>
            val regCode = if (List("qp", "q") contains op.operandType.code) "o" else op.operandType.code // for some reason the code for 64-bit is "ro"
            Seq(getOpcodeString + " + r" + regCode + "\n")
          case _ => Seq(getOpcodeString + "\n")
          //}

        }
      }.getOrElse(Seq(getOpcodeString + "\n"))

      val prefix: Seq[String] = getOperand.map { op =>
        op match {
          case OperandInstance(address, operandType, size, _) =>
            if (operandType.isInstanceOf[FixedOperandType] && operandType.asInstanceOf[FixedOperandType].promotedByRex && op.operandSize.size == 64) {
              Seq("override def prefix = REX.W(true)\n")
            } else {
              Nil
            }
          case _ => Nil
        }
      }.getOrElse(Nil)

      val implicitOp: Seq[String] = if (entry.syntax.exists { syn => syn.hasImplicit }) {
        Seq("override def hasImplicitOperand = true\n")
      } else {
        Nil
      }
      val footer = "}"
      header +: (Seq(opcodeString, prefix, Seq(getFormat), implicitOp).flatten.map(x => "  " + x) :+ footer)
    }
  }

  case class x86ZeroOperandInstruction(opcode: Int,
                                       numOpcodeBytes: Int,
                                       mnemonic: String,
                                       entry: x86Entry) extends InstructionInstance {

    def getOperand = None

    def getClassHeader(name: String): String = {
      val result = s"implicit object $name extends NoOp{\n"
      result
    }

    def hasImplicitOperand: Boolean = false
    def getFormat = ""

    def getSize: Int = 0
  }

  case class x86OneOperandInstruction(opcode: Int,
                                      numOpcodeBytes: Int,
                                      mnemonic: String,
                                      operand: OperandInstance,
                                      entry: x86Entry) extends InstructionInstance {

    def getOperand = Some(operand)

    def getClassHeader(name: String): String = {
      val result = s"implicit object $name extends OneOp[" + operand + "] {\n"
      result
    }

    def hasImplicitOperand: Boolean = {
      operand.isImplicit
    }
    
    def getFormat = {
        if (List("r8", "r16", "r32", "r64").contains(operand.toString)) {
          "val format = RegFormat\n"
        } else if (List("rm8", "rm16", "rm32", "rm64", "m8", "m16", "m32", "m64", "m128", "m", "moffs8", "moffs16", "moffs32", "moffs64").contains(operand.toString)) {
          "val format = RmFormat\n"
        } else {
          "val format = ImmFormat\n"
        }
    }

    def getSize: Int = {
      val modSize = if (entry.hasModRMByte) 1 else 0
      1 + modSize + operand.operandSize.size / 8
    }
  }

  case class x86TwoOperandInstruction(opcode: Int,
                                      numOpcodeBytes: Int,
                                      mnemonic: String,
                                      operands: TwoOperandInstance,
                                      entry: x86Entry) extends InstructionInstance {

    def getClassHeader(name: String): String = {
      val result = s"implicit object $name extends TwoOp[${operands._1}, ${operands._2}] {\n"
      result
    }

    def getOperand = Some(operands._1)

    def hasImplicitOperand = false
    
    def getFormat = {
      if (List("r8", "r16", "r32", "r64", "Sreg").contains(operands._1.toString) &&
          List("rm8", "rm16", "rm32", "rm64", "r8", "r16", "r32", "r64", "m8", "m16", "m32", "m64", "m128", "m", "moffs8", "moffs16", "moffs32", "moffs64").contains(operands._2.toString)) {
        "val format = RegRmFormat\n"
      } else if (List("r8", "r16", "r32", "r64", "Sreg").contains(operands._2.toString) &&
          List("rm8", "rm16", "rm32", "rm64", "r8", "r16", "r32", "r64", "m8", "m16", "m32", "m64", "m128", "m", "moffs8", "moffs16", "moffs32", "moffs64").contains(operands._1.toString)) {     
        "val format = MemRegFormat\n"
      } else if (List("rm8", "rm16", "rm32", "rm64", "r8", "r16", "r32", "r64", "m8", "m16", "m32", "m64", "m128", "m", "moffs8", "moffs16", "moffs32").contains(operands._1.toString)) {     
        "val format = RmImmFormat\n"
      } else {
        "val format = null\n"
      }
    }

    def getSize: Int = {
      val modSize = if (entry.hasModRMByte) 1 else 0
      1 + modSize + operands._2.operandSize.size / 8
    }
  }

  case class x86InstructionDef(opcode: Int,
                               numOpcodeBytes: Int,
                               mnemonic: String,
                               operands: Seq[OperandDef],
                               entry: x86Entry) {
    def getInstances: Seq[InstructionInstance] = {
      if (operands.size == 2) {
        //if (operands(0).operandType.isDefined &&
        //  operands(1).operandType.isDefined) {
        val ops = TwoOperandDef(operands(0), operands(1))
        ops.getInstances.map { instance => x86TwoOperandInstruction(opcode, numOpcodeBytes, mnemonic, instance, entry) }
        //} else if (!operands(1).operandType.isDefined) { // implicit
        //  operands(0).getInstances.map{instance => x86OneOperandInstruction(mnemonic + "_" + opcode + "_" + instance, opcode, mnemonic, instance, entry)}
        //} else {
        //  Nil
      } else if (operands.size == 1) {
        operands(0).getInstances.map { instance => x86OneOperandInstruction(opcode, numOpcodeBytes, mnemonic, instance, entry) }
      } else if (operands.isEmpty) {
        Seq(x86ZeroOperandInstruction(opcode, numOpcodeBytes, mnemonic, entry))
      } else {
        Nil
      }
    }

  }

  case class x86Opcode(opcode: Int,
                       entries: Seq[x86Entry],
                       numOpcodeBytes: Int)

  case class x86Entry(mode: Option[String],
                      syntax: Seq[SyntaxDef],
                      opcodeEx: Option[Int],
                      opsize: Option[Boolean],
                      direction: Option[Boolean],
                      hasRegisterInModRM: Boolean,
                      hasModRMByte: Boolean,
                      brief: String,
                      group1: Option[String],
                      group2: Option[String],
                      group3: Option[String])

  case class SyntaxDef(mnemonic: String,
                       operands: Seq[OperandDef],
                       hasImplicit: Boolean)

  case class TwoOperandDef(operand1: OperandDef, operand2: OperandDef) {

    def zipSizes(op1Sizes: Seq[(OperandType, OperandSize)], op2Sizes: Seq[(OperandType, OperandSize)]): Seq[TwoOperandInstance] = {
      op1Sizes.zip(op2Sizes).map { x =>
        val op1 = OperandInstance(
          operand1.addressingMethod,
          x._1._1,
          x._1._2,
          operand1.explicitOperandName)
        val op2 = OperandInstance(
          operand2.addressingMethod,
          x._2._1,
          x._2._2,
          operand2.explicitOperandName)
        TwoOperandInstance(op1, op2)
      }
    }

    def getInstances: Seq[TwoOperandInstance] = {
      //if (operand1.operandType.isDefined && operand2.operandType.isDefined) {
        val op1Sizes: Seq[(OperandType, OperandSize)] = operand1.operandType match {
          case Some(CompositeOperandType(_, _, components, _)) => components map { size => (OperandType.decodeOperandType(size), OperandType.decodeOperandType(size).asInstanceOf[FixedOperandType].size) }
          case Some(FixedOperandType(_, _, size, _, _)) => Seq((operand1.operandType.get, size))
          case _ => {
            operand1.addressingMethod match {
              case Some(ModRMByteMemoryOnly) =>
                Seq((null, NoSize))
                
              case _ => Seq()
            }
          }
        }
        val op2Sizes: Seq[(OperandType, OperandSize)] = operand2.operandType match {
          case Some(CompositeOperandType(_, _, components, _)) => components map { size => (OperandType.decodeOperandType(size), OperandType.decodeOperandType(size).asInstanceOf[FixedOperandType].size) }
          case Some(FixedOperandType(_, _, size, _, _)) => Seq((operand2.operandType.get, size))
          case _ => {
            operand2.addressingMethod match {
              case Some(ModRMByteMemoryOnly) =>
                Seq((null, NoSize))
                
              case _ => Seq()
            }
          }
        }

        (op1Sizes.length,
          op2Sizes.length) match {

            case (_, 1) => {
              for {
                (opType, size1) <- op1Sizes
                (opType2, size2) <- op2Sizes
              } yield {
                val op1 = OperandInstance(
                  operand1.addressingMethod,
                  opType,
                  size1,
                  operand1.explicitOperandName)
                val op2 = OperandInstance(
                  operand2.addressingMethod,
                  opType2,
                  size2,
                  operand2.explicitOperandName)
                TwoOperandInstance(op1, op2)
              }
            }
            case (1, _) => {
              for {
                (opType, size1) <- op1Sizes
                (opType2, size2) <- op2Sizes
              } yield {
                val op1 = OperandInstance(
                  operand1.addressingMethod,
                  opType,
                  size1,
                  operand1.explicitOperandName)
                val op2 = OperandInstance(
                  operand2.addressingMethod,
                  opType2,
                  size2,
                  operand2.explicitOperandName)
                TwoOperandInstance(op1, op2)
              }
            }
            case (x, y) if x == y => {
              zipSizes(op1Sizes, op2Sizes)
            }
            case (3, 2) =>
              val padded = op2Sizes :+ op2Sizes.last
              zipSizes(op1Sizes, padded)
            case (2, 3) =>
              val padded = op1Sizes :+ op1Sizes.last
              zipSizes(padded, op2Sizes)
            case _ =>
              Seq()
      }
    }
  }

  case class OperandDef(srcOrDst: String,
                        operandType: Option[OperandType],
                        addressingMethod: Option[AddressingMethod],
                        explicitOperandName: Option[String]) {

    def getInstances: Seq[OperandInstance] = {
      if (operandType.isDefined) {
        val opSizes: Seq[(OperandType, OperandSize)] = operandType match {
          case Some(CompositeOperandType(_, _, sizes, _)) => sizes map { size => (OperandType.decodeOperandType(size), OperandType.decodeOperandType(size).asInstanceOf[FixedOperandType].size) }
          case Some(FixedOperandType(_, _, size, _, _)) => Seq((operandType.get, size))
          case _ => Seq()
        }
        for {
          (optype, size) <- opSizes
        } yield {
          OperandInstance(
            addressingMethod,
            optype,
            size,
            explicitOperandName)
        }

      } else {
        addressingMethod match {
          case Some(ModRMByteMemoryOnly) =>
            Seq(OperandInstance(
              addressingMethod,
              null,
              NoSize,
              explicitOperandName))
          case _ => Seq()
        }
      }
    }
  }

  case class OperandInstance(addressingMethod: Option[AddressingMethod],
                             operandType: OperandType,
                             operandSize: OperandSize,
                             explicitOperandName: Option[String]) {
    override def toString = {
      
      def addy = addressingMethod.map { addy => addy.toString }.getOrElse("")
      def size = if (addy != "Sreg") operandSize.toString else "" 
      
      val result = explicitOperandName.map { name => 
        if (name == "rAX") {
          operandSize.size match {
            case 16 => "AX"
            case 32 => "EAX"
            case 64 => "RAX"
            case _ => "ERROR"
          }
        } else {
          name
        }
      }.getOrElse(addy + size)
      
      if (result == "STi/m32") { // hack for now!!! x87
        "m32"
      } else {
        result
      }
    }
    def isImplicit = false
  }

  case class TwoOperandInstance(_1: OperandInstance, _2: OperandInstance)

  def getOptionalBoolean(node: NodeSeq): Option[Boolean] = {
    if (!node.isEmpty) Some(if (node.text == "0") false else true) else None
  }

  def getOptionalInt(node: NodeSeq): Option[Int] = {
    if (!node.isEmpty) Some(node.text.toInt) else None
  }

  def getOptionalString(node: NodeSeq): Option[String] = {
    if (!node.isEmpty) Some(node.text) else None
  }
  
  def isImplicit(node: Node): Boolean = {
    val isDisplayed = if (!(node \ "@displayed").isEmpty) (node \@ "displayed") == "yes" else true
    //val hasNoInfo = (node \ "@type").isEmpty && (node \ "@group").isEmpty && (node \ "a").isEmpty && (node \ "t").isEmpty
    !isDisplayed || (node \ "a").isEmpty
  }

  def parseSyntax(entry: NodeSeq): Seq[SyntaxDef] = {
    (entry \ "syntax").map { syntax =>
      val mnemonic = (syntax \ "mnem").text
      val operands = (syntax \ "_").filter { node => node.label != "mnem" }
      var hasImplicit = false
      val explicitOperands = operands filter{node => !isImplicit(node)}
         
      val ops = explicitOperands map { operand =>
        val hasDetails = !(operand \ "a").isEmpty

        val opType =
          if (!(operand \ "t").isEmpty && (operand \ "t").text.trim != "")
            Some(OperandType.decodeOperandType((operand \ "t").text.trim))
          else if (!(operand \ "@type").isEmpty)
            Some(OperandType.decodeOperandType((operand \ "@type").text.trim))
          else {
            None
          }

        val opAddressing =
          if (!(operand \ "a").isEmpty)
            Some(AddressingMethod.decodeAddressingMethod((operand \ "a").text.trim))
          else if (!(operand \ "@address").isEmpty)
            Some(AddressingMethod.decodeAddressingMethod((operand \ "@address").text.trim))
          else
            None

        val opName = if (!(operand \ "@type").isEmpty || !(operand \ "@address").isEmpty) {
          Some(operand.text)
        } else {
          None
        }
            
        OperandDef(operand.label, opType, opAddressing, opName)
      }

      val hasImplicitOperand = explicitOperands.size != operands.size
      
      SyntaxDef(mnemonic, ops, hasImplicitOperand)
    }
  }

  def parseEntry(entry: NodeSeq): x86Entry = {
    val mode = getOptionalString(entry \ "@mode")
    val opcodeEx = getOptionalInt(entry \ "opcd_ext")
    val opSize = getOptionalBoolean(entry \ "@opsize")
    val direction = getOptionalBoolean(entry \ "@direction")
    val grp1 = getOptionalString(entry \ "grp1")
    val grp2 = getOptionalString(entry \ "grp2")
    val grp3 = getOptionalString(entry \ "grp3")
    val isRegister = (entry \@ "r") == "yes"
    val note = (entry \ "note" \ "brief")
    val brief = if (note.isEmpty) "" else note.text

    val operandDefs = parseSyntax(entry)

    // seems to be pretty simple
    val hasModRMByte = isRegister || opcodeEx.isDefined

    x86Entry(mode, operandDefs, opcodeEx, opSize, direction, isRegister, hasModRMByte, brief, grp1, grp2, grp3)
  }

  def loadXML(): Seq[x86InstructionDef] = {
    val xml = XML.loadFile("x86reference.xml")
    val one_opcodes = (xml \ "one-byte" \\ "pri_opcd")
    val two_opcodes = (xml \ "two-byte" \\ "pri_opcd")

    val opcodes = one_opcodes.map { pri_opcode =>
      val nonAliasedEntries = (pri_opcode \ "entry").filter { entry => (entry \ "@alias").size == 0} // && ((entry \ "proc_start").size == 0 || (entry \ "proc_start")(0).text == "01" || (entry \ "proc_start")(0).text == "10")}
      val opcode = Integer.parseInt(pri_opcode \@ "value", 16)
      x86Opcode(opcode, nonAliasedEntries.map(parseEntry), 1)
    } ++ two_opcodes.map { pri_opcode =>
      val nonAliasedEntries = (pri_opcode \ "entry").filter { entry => (entry \ "@alias").size == 0} // && ((entry \ "proc_start").size == 0 || (entry \ "proc_start")(0).text == "01" || (entry \ "proc_start")(0).text == "10")}
      val opcode = Integer.parseInt(pri_opcode \@ "value", 16)
      x86Opcode(opcode, nonAliasedEntries.map(parseEntry), 2)
    }

    var lastEntry: x86Entry = null

    val result = opcodes.flatMap { op =>
      op.entries.flatMap { entry =>
        entry.syntax.map { syntax =>
          x86InstructionDef(op.opcode, op.numOpcodeBytes, syntax.mnemonic, syntax.operands, entry)
        }
      }
    }
    
    val mnemonicMap = result.groupBy { x => x.mnemonic }
    
    println("Loading XML..." + opcodes.size + " opcodes read..." + mnemonicMap.size + " mnemonics read...")
    
    result
  }

  def outputInstructionFile(mnemonic: String, instructions: LinkedHashSet[InstructionInstance], folder: String): Seq[InstructionInstance] = {
    val newFolder = new File("../scala-x86-inst/src/main/scala/com/scalaAsm/x86/Instructions/" + folder)
    if (!newFolder.exists()) newFolder.mkdirs()
    val writer = new PrintWriter("../scala-x86-inst/src/main/scala/com/scalaAsm/x86/Instructions/" + folder + "/" + mnemonic + ".scala", "UTF-8");

    val outputOrder = new ListBuffer[InstructionInstance]()
    
    def getOperandDescriptor(mnemonic: String) = {
      val hasZeroOperandEntry = instructions.exists{inst => inst.isInstanceOf[x86ZeroOperandInstruction]}   
      val hasOneOperandEntry = instructions.exists{inst => inst.isInstanceOf[x86OneOperandInstruction]} 
      val hasTwoOperandsEntry = instructions.exists{inst => inst.isInstanceOf[x86TwoOperandInstruction]}
      
      val desc = Seq(
        if (hasZeroOperandEntry) Some(s"ZeroOperands[$mnemonic]") else None,
        if (hasOneOperandEntry) Some(s"OneOperand[$mnemonic]") else None,
        if (hasTwoOperandsEntry) Some(s"TwoOperands[$mnemonic]") else None)
      
      val results = desc.flatMap{x => x}
      if (results.isEmpty) {
        ""
      } else {
        desc.flatMap{x => x}.reduce{_ + " with " + _}
      }
    }
    
    // assume all instructions for a mnemonic have the same numOpcodeByte values.  I've investigated - this is safe!
    val numOpcodeBytes = instructions.head.numOpcodeBytes

    // must do this to resolve (rm, r) (r, rm) ambiguous implicit resolution.  A little hacky
    val (low, high) = instructions.partition { inst =>
      inst match {
        case x86TwoOperandInstruction(_, _, _, operands, _) if operands._1.addressingMethod.isDefined && operands._2.addressingMethod.isDefined =>
          val is64 = operands._2.addressingMethod.get.abbreviation == "rm" && operands._2.operandSize == _64 &&
            operands._1.addressingMethod.get.abbreviation == "r" && operands._1.operandSize == _64
          val is32 = operands._2.addressingMethod.get.abbreviation == "rm" && operands._2.operandSize == _32 &&
            operands._1.addressingMethod.get.abbreviation == "r" && operands._1.operandSize == _32
          val is16 = operands._2.addressingMethod.get.abbreviation == "rm" && operands._2.operandSize == _16 &&
            operands._1.addressingMethod.get.abbreviation == "r" && operands._1.operandSize == _16
          val hasRM = operands._2.addressingMethod.get.abbreviation == "rm" || operands._1.addressingMethod.get.abbreviation == "rm"
          hasRM && !(is64 || is32 || is16)
        case x86OneOperandInstruction(_, _, _, operand, _) if operand.addressingMethod.isDefined && operand.addressingMethod.isDefined =>
          operand.addressingMethod.get.abbreviation == "rm"
        case _ => false
      }
    }
    
    val lowInst = low.zipWithIndex
    val highInst = high.zipWithIndex.map{case(inst,index) => (inst, index + lowInst.size)}

    val desc = instructions.map(inst => inst.entry.brief).toSet.reduce(_ + ", " + _)
    val category = "general" + instructions.head.entry.group2.map(grp2 => "/" + grp2 + instructions.head.entry.group3.map(grp3 => "/" + grp3).getOrElse("")).getOrElse("")
    
    writer.println("package com.scalaAsm.x86");
    writer.println("package Instructions");
    writer.println("package " + folder.replace('/', '.'));
    writer.println("")
    
    if (desc != "") {
      writer.println("// Description: " + desc)
    }
    
    writer.println("// Category: " + category + "\n")
    
    val opcodeType = if (numOpcodeBytes == 1) "OneOpcode" else "TwoOpcodes"
      
    writer.println(s"trait ${mnemonic.toUpperCase()} extends InstructionDefinition {")
    writer.println("  val mnemonic = \"" + mnemonic + "\"")
    writer.println("}")
    writer.println("")
    writer.println(s"object $mnemonic extends ${getOperandDescriptor(mnemonic)} with ${mnemonic.toUpperCase()}Impl")
    writer.println("")
    
    if (!low.isEmpty && !high.isEmpty) {
      writer.println(s"trait ${mnemonic.toUpperCase()}Low extends $mnemonic {")
      val descriptions = Set[String]()
      for ((inst, index) <- lowInst) {
        writer.println(inst.generateClass("_" + index).map(x => "  " + x).mkString)
        outputOrder += inst
        if (inst != low.last)
          writer.println("") 
      }
      writer.println("}\n")

      writer.println(s"trait ${mnemonic.toUpperCase()}Impl extends ${mnemonic.toUpperCase()}Low {")
      for ((inst, index) <- highInst) {
        writer.println(inst.generateClass("_" + index).map(x => "  " + x).mkString)
        outputOrder += inst
        if (inst != high.last)
          writer.println("")
      }
      writer.println("}")
    } else {
      writer.println(s"trait ${mnemonic.toUpperCase()}Impl extends $mnemonic {")
      for ((inst, index) <- instructions.zipWithIndex) {
        writer.println(inst.generateClass("_" + index).map(x => "  " + x).mkString)
        outputOrder += inst
        if (inst != instructions.last)
          writer.println("")
      }
      writer.println("}")
    }
    writer.close();
    outputOrder.toSeq
  }

  def main(args: Array[String]): Unit = {
    try {
      val instMap = scala.collection.mutable.Map[String, InstructionInstance]()
      
      println("Generating x86 instructions...")
      val insts = loadXML().flatMap { x => x.getInstances }
      println(insts.size + " instruction instances generated!")
      val genFiles = insts.filter(inst => inst.entry.group1.getOrElse("") == "gen").groupBy { x => x.mnemonic }
      genFiles.foreach{ 
           case (mnem, insts)  => { 
             val uniqueInst = LinkedHashSet[InstructionInstance]()
             uniqueInst ++= insts
             val outputInsts = outputInstructionFile(mnem, uniqueInst, "General")
             outputInsts.zipWithIndex.foreach { case (inst, index) =>
                instMap += inst.mnemonic + "._" + index -> inst
             }
           }
           case _ =>
         }
      
      val x87Files = insts.filter(inst => inst.entry.group1.getOrElse("") == "x87fpu").groupBy { x => x.mnemonic }
      x87Files.foreach{ 
           case (mnem, insts)  => { 
             val uniqueInst = LinkedHashSet[InstructionInstance]()
             uniqueInst ++= insts
             val outputInsts = outputInstructionFile(mnem, uniqueInst, "x87")
             outputInsts.zipWithIndex.foreach { case (inst, index) =>
                instMap += inst.mnemonic + "._" + index -> inst
             }
           }
           case _ =>
         }
      
      val systemFiles = insts.filter(inst => inst.entry.group1.getOrElse("") == "system" && inst.mnemonic != "MOV").groupBy { x => x.mnemonic }
      systemFiles.foreach{ 
           case (mnem, insts)  => { 
             val uniqueInst = LinkedHashSet[InstructionInstance]()
             uniqueInst ++= insts
             val outputInsts = outputInstructionFile(mnem, uniqueInst, "System")
             outputInsts.zipWithIndex.foreach { case (inst, index) =>
                instMap += inst.mnemonic + "._" + index -> inst
             }
           }
           case _ =>
         }
      
      
      
      println(genFiles.size + x87Files.size + systemFiles.size + " files generated!")
      println("Done generating instructions!")
      
      val groups = instMap.groupBy{case (name, inst) => if (inst.numOpcodeBytes == 1)  inst.opcode else 0x0F00 + inst.opcode}.map{case (opcode, insts) => opcode -> insts.keySet.toSet}
      val sorted: SortedMap[Int, Set[String]] = SortedMap.empty[Int, Set[String]] ++ groups
      
      val writer = new PrintWriter("../scala-x86-inst/src/main/scala/com/scalaAsm/x86/Instructions/InstructionMap.scala", "UTF-8");
      
      
      writer.println("package com.scalaAsm.x86.Instructions")
      writer.println("")
      writer.println("import com.scalaAsm.x86.Instructions.General._")
      writer.println("import com.scalaAsm.x86.Instructions.System._")
      writer.println("import com.scalaAsm.x86.Instructions.x87._")
      writer.println("")
      writer.println("object InstructionMap {")
      writer.println("  val instMap = Map[Int, Set[x86Instruction]](")
      writer.println(sorted.map{ case (opcode, insts) => {
        val hex = Integer.toHexString(opcode)
        if (hex.size%2 == 1)
          s"    0x0$hex -> $insts"
        else
          s"    0x$hex -> $insts"
      }}.reduce{_ + ",\n" + _} + ")")
      writer.println("}")
      writer.close();
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

}