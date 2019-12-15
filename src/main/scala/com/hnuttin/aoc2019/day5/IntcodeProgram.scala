package com.hnuttin.aoc2019.day5

import com.hnuttin.aoc2019.day5.ParameterMode.ParameterMode

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

	def getParameter(paramPosition: Int, parameterMode: ParameterMode): Int = {
		val param = codes(instructionPointer + paramPosition)
		parameterMode match {
			case ParameterMode.POSITION => codes(param)
			case ParameterMode.IMMEDIATE => param
		}
	}

	def transformAndIncrementPointer(positionToReplace: Int, value: Int, pointerIncrement: Int): IntcodeProgram = {
		new IntcodeProgram(codes.updated(positionToReplace, value), instructionPointer + pointerIncrement, output)
	}

	def transformWithInputAndIncrementPointer(input: Int, positionToReplace: Int, pointerIncrement: Int): IntcodeProgram = {
		new IntcodeProgram(codes.updated(positionToReplace, input), instructionPointer + pointerIncrement, output)
	}

	def incrementPointer(pointerIncrement: Int): IntcodeProgram = {
		new IntcodeProgram(codes, instructionPointer + pointerIncrement, output)
	}

	def incrementPointerAndAddOutput(pointerIncrement: Int, newOutput: Int): IntcodeProgram = {
		new IntcodeProgram(codes, instructionPointer + pointerIncrement, Option(newOutput))
	}

	def setPointer(instructionPointer: Int): IntcodeProgram = {
		new IntcodeProgram(codes, instructionPointer, output)
	}

}