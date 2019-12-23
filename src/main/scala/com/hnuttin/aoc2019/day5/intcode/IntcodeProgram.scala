package com.hnuttin.aoc2019.day5.intcode

import com.hnuttin.aoc2019.day5.intcode.ParameterMode.ParameterMode

import scala.collection.immutable.HashMap

class IntcodeProgram(private[intcode] var memory: Map[Long, Long]) {

	private[intcode] var instructionPointer = 0L
	private[intcode] var relativeBase = 0L

	private[intcode] var _halted = false

	def halted: Boolean = _halted

	def execute(inputSupplier: () => Long, outputHandler: Long => Unit): Unit = {
		executeRecursive(inputSupplier, outputHandler)
	}

	@scala.annotation.tailrec
	private def executeRecursive(inputSupplier: () => Long, outputHandler: Long => Unit): Unit = {
		Opcode.parse(memory(instructionPointer.intValue)).operate(this, inputSupplier, outputHandler)
		if (!halted) {
			executeRecursive(inputSupplier, outputHandler)
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

	def emptyInputSupplier: () => Long = () => 0L

	def noopOutputHandler: Long => Unit = (_: Long) => ()
}