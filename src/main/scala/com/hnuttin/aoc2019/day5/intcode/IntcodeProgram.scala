package com.hnuttin.aoc2019.day5.intcode

import com.hnuttin.aoc2019.day5.intcode.ParameterMode.ParameterMode

import scala.collection.immutable.HashMap

class IntcodeProgram private(val memory: Map[Long, Long], val instructionPointer: Long, val relativeBase: Long, val output: Option[Long]) {

	def executeUntilHalted(inputs: List[Long]): Long = {
		Opcode.parse(memory(instructionPointer.intValue)).executeUntilHalted(this, inputs)
	}

	def executeUntilOutputOrHalted(inputs: List[Long]): (Option[Long], Option[IntcodeProgram]) = {
		Opcode.parse(memory(instructionPointer.intValue)).executeUntilOutputOrHalted(this, inputs)
	}

	private def this(memory: Map[Long, Long]) {
		this(memory, 0, 0, Option.empty)
	}

	private[intcode] def getParameter(paramPosition: Long, parameterMode: ParameterMode): Long = {
		val param = memory((instructionPointer + paramPosition).intValue)
		parameterMode match {
			case ParameterMode.POSITION => readFromMemory(param)
			case ParameterMode.IMMEDIATE => param
			case ParameterMode.RELATIVE => readFromMemory(param + relativeBase)
		}
	}

	private def readFromMemory(address: Long): Long = {
		memory.getOrElse(address, 0)
	}

	private[intcode] def transformAndIncrementPointer(positionToReplace: Long, value: Long, pointerIncrement: Long): IntcodeProgram = {
		new IntcodeProgram(memory + (positionToReplace -> value), instructionPointer + pointerIncrement, relativeBase, output)
	}

	private[intcode] def incrementPointer(pointerIncrement: Long): IntcodeProgram = {
		new IntcodeProgram(memory, instructionPointer + pointerIncrement, relativeBase, output)
	}

	private[intcode] def incrementPointerAndSetOutput(pointerIncrement: Long, newOutput: Long): IntcodeProgram = {
		new IntcodeProgram(memory, instructionPointer + pointerIncrement, relativeBase, Option(newOutput))
	}

	private[intcode] def setPointer(instructionPointer: Long): IntcodeProgram = {
		new IntcodeProgram(memory, instructionPointer, relativeBase, output)
	}

	private[intcode] def clearOutput(): IntcodeProgram = {
		new IntcodeProgram(memory, instructionPointer, relativeBase, Option.empty)
	}

	private[intcode] def incrementRelativeBaseAndIncrementPointer(relativeBaseIncrement: Long, pointerIncrement: Long): IntcodeProgram = {
		new IntcodeProgram(memory, instructionPointer + pointerIncrement, relativeBase + relativeBaseIncrement, output)
	}

}

object IntcodeProgram {
	def fromIntcode(intcode: List[Int]): IntcodeProgram = {
		fromLongCode(intcode.map(code => code.longValue))
	}

	def fromLongCode(longcode: List[Long]): IntcodeProgram = {
		val memory = longcode
				.zipWithIndex
				.foldLeft(HashMap[Long, Long]())(foldToMemory)
		new IntcodeProgram(memory)
	}

	private def foldToMemory(accum: HashMap[Long, Long], tupple: Tuple2[Long, Int]): HashMap[Long, Long] = {
		accum + (tupple._2.longValue -> tupple._1)
	}
}