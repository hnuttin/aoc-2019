package com.hnuttin.aoc2019.day5.intcode

import com.hnuttin.aoc2019.day5.intcode.ParameterMode.ParameterMode

class IntcodeProgram private(val codes: List[Int], val instructionPointer: Int, val output: Option[Int]) {

	def this(codes: List[Int]) {
		this(codes, 0, Option.empty)
	}

	def executeUntilHalted(inputs: List[Int]): Int = {
		Opcode.parse(codes(instructionPointer)).executeUntilHalted(this, inputs)
	}

	def executeUntilOutputOrHalted(inputs: List[Int]): (Option[Int], Option[IntcodeProgram]) = {
		Opcode.parse(codes(instructionPointer)).executeUntilOutputOrHalted(this, inputs)
	}

	private[intcode] def getParameter(paramPosition: Int, parameterMode: ParameterMode): Int = {
		val param = codes(instructionPointer + paramPosition)
		parameterMode match {
			case ParameterMode.POSITION => codes(param)
			case ParameterMode.IMMEDIATE => param
		}
	}

	private[intcode] def transformAndIncrementPointer(positionToReplace: Int, value: Int, pointerIncrement: Int): IntcodeProgram = {
		new IntcodeProgram(codes.updated(positionToReplace, value), instructionPointer + pointerIncrement, output)
	}

	private[intcode] def transformWithInputAndIncrementPointer(input: Int, positionToReplace: Int, pointerIncrement: Int): IntcodeProgram = {
		new IntcodeProgram(codes.updated(positionToReplace, input), instructionPointer + pointerIncrement, output)
	}

	private[intcode] def incrementPointer(pointerIncrement: Int): IntcodeProgram = {
		new IntcodeProgram(codes, instructionPointer + pointerIncrement, output)
	}

	private[intcode] def incrementPointerAndSetOutput(pointerIncrement: Int, newOutput: Int): IntcodeProgram = {
		new IntcodeProgram(codes, instructionPointer + pointerIncrement, Option(newOutput))
	}

	private[intcode] def setPointer(instructionPointer: Int): IntcodeProgram = {
		new IntcodeProgram(codes, instructionPointer, output)
	}

	private[intcode] def clearOutput(): IntcodeProgram = {
		new IntcodeProgram(codes, instructionPointer, Option.empty)
	}

}