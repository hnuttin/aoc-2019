package com.hnuttin.aoc2019.day5

import com.hnuttin.aoc2019.day5.ParameterMode.ParameterMode

class IntcodeProgram(val codes: List[Int], instructionPointer: Int) {

	def execute(): Unit = {
		Opcode.parse(codes(instructionPointer))
				.operate(this)
				.foreach(intcodeProgram => intcodeProgram.execute())
	}

	def getParameter(paramPosition: Int, parameterMode: ParameterMode): Int = {
		val param = codes(instructionPointer + paramPosition)
		parameterMode match {
			case ParameterMode.POSITION => codes(param)
			case ParameterMode.IMMEDIATE => param
		}
	}

	def transformAndIncrementPointer(positionToReplace: Int, value: Int, pointerIncrement: Int): IntcodeProgram = {
		new IntcodeProgram(codes.updated(positionToReplace, value), instructionPointer + pointerIncrement)
	}

	def incrementPointer(pointerIncrement: Int): IntcodeProgram = {
		new IntcodeProgram(codes, instructionPointer + pointerIncrement)
	}

}