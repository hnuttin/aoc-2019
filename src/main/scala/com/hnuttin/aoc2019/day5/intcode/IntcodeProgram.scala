package com.hnuttin.aoc2019.day5.intcode

import com.hnuttin.aoc2019.day5.intcode.ParameterMode.ParameterMode

import scala.collection.immutable.HashMap

class IntcodeProgram(private[intcode] var memory: Map[Long, Long]) {

	private[intcode] var instructionPointer = 0L
	private[intcode] var relativeBase = 0L
	private[intcode] var _outputs: List[Long] = List()

	private[intcode] var _halted = false

	def halted: Boolean = _halted

	def outputs: List[Long] = _outputs

	def executeUntilHalted(): Unit = {
		executeUntilHalted(List())
	}

	def executeUntilHalted(inputs: List[Long]): Unit = {
		executeUntilHaltedRecursive(inputs)
	}

	@scala.annotation.tailrec
	private def executeUntilHaltedRecursive(inputs: List[Long]): List[Long] = {
		val newInputs = Opcode.parse(memory(instructionPointer.intValue)).operate(this, inputs)
		if (halted) {
			inputs
		} else {
			executeUntilHaltedRecursive(newInputs)
		}
	}

	def executeUntilOutputOrHalted(): Unit = {
		executeUntilOutputOrHalted(List())
	}

	def executeUntilOutputOrHalted(inputs: List[Long]): Unit = {
		executeUntilOutputOrHaltedRecursive(inputs)
	}

	@scala.annotation.tailrec
	private def executeUntilOutputOrHaltedRecursive(inputs: List[Long]): List[Long] = {
		val outputsBefore = _outputs
		val newInputs = Opcode.parse(memory(instructionPointer.intValue)).operate(this, inputs)
		if (halted || outputsBefore.length < _outputs.length) {
			newInputs
		} else {
			executeUntilOutputOrHaltedRecursive(newInputs)
		}
	}

	private[intcode] def getParameterAsValue(paramPosition: Long, parameterMode: ParameterMode): Long = {
		val param = memory((instructionPointer + paramPosition).intValue)
		parameterMode match {
			case ParameterMode.POSITION => readFromMemory(param)
			case ParameterMode.IMMEDIATE => param
			case ParameterMode.RELATIVE => readFromMemory(param + relativeBase)
		}
	}

	private def readFromMemory(address: Long): Long = {
		if (address < 0) throw new IllegalArgumentException
		memory.getOrElse(address, 0)
	}

	private[intcode] def getParameterAsPosition(paramPosition: Long, parameterMode: ParameterMode): Long = {
		val param = memory((instructionPointer + paramPosition).intValue)
		parameterMode match {
			case ParameterMode.POSITION => param
			case ParameterMode.IMMEDIATE => param
			case ParameterMode.RELATIVE => param + relativeBase
		}
	}

}

object IntcodeProgram {
	def fromIntcode(intcode: List[Int]): IntcodeProgram = {
		fromLongCode(intcode.map(code => code.longValue))
	}

	def fromLongCode(longcode: List[Long]): IntcodeProgram = {
		val initialMemory = longcode
				.zipWithIndex
				.foldLeft(HashMap[Long, Long]())(foldToMemory)
		new IntcodeProgram(initialMemory)
	}

	private def foldToMemory(accum: HashMap[Long, Long], tupple: Tuple2[Long, Int]): HashMap[Long, Long] = {
		accum + (tupple._2.longValue -> tupple._1)
	}
}