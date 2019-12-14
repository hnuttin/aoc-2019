package com.hnuttin.aoc2019.day5

import com.hnuttin.aoc2019.day5.ParameterMode.ParameterMode

class IntcodeProgram private(val codes: List[Int], val instructionPointer: Int, val inputs: List[Int], val output: Option[Int]) {

	def this(codes: List[Int], inputs: List[Int]) {
		this(codes, 0, inputs: List[Int], Option.empty)
	}

	def executeUntilHalted(): Int = {
		val newProgram = Opcode.parse(codes(instructionPointer)).operate(this)
		if (newProgram.isDefined) newProgram.get.executeUntilHalted() else output.getOrElse(0)
	}

	def executeUntilOutputOrHalted(): (Option[Int], Option[IntcodeProgram]) = {
		val newProgram = Opcode.parse(codes(instructionPointer)).operate(this)
		if (newProgram.isDefined) {
			if (newProgram.get.output.isDefined) {
				Tuple2(newProgram.get.output, newProgram)
			} else {
				newProgram.get.executeUntilOutputOrHalted()
			}
		} else {
			Tuple2(output, Option.empty)
		}
	}

	def getParameter(paramPosition: Int, parameterMode: ParameterMode): Int = {
		val param = codes(instructionPointer + paramPosition)
		parameterMode match {
			case ParameterMode.POSITION => codes(param)
			case ParameterMode.IMMEDIATE => param
		}
	}

	def transformAndIncrementPointer(positionToReplace: Int, value: Int, pointerIncrement: Int): IntcodeProgram = {
		new IntcodeProgram(codes.updated(positionToReplace, value), instructionPointer + pointerIncrement, inputs, output)
	}

	def transformWithInputAndIncrementPointer(positionToReplace: Int, pointerIncrement: Int): IntcodeProgram = {
		new IntcodeProgram(codes.updated(positionToReplace, inputs.head), instructionPointer + pointerIncrement, inputs.tail, output)
	}

	def incrementPointer(pointerIncrement: Int): IntcodeProgram = {
		new IntcodeProgram(codes, instructionPointer + pointerIncrement, inputs, output)
	}

	def incrementPointerAndAddOutput(pointerIncrement: Int, output: Int): IntcodeProgram = {
		new IntcodeProgram(codes, instructionPointer + pointerIncrement, inputs, Option(output))
	}

	def setPointer(instructionPointer: Int): IntcodeProgram = {
		new IntcodeProgram(codes, instructionPointer, inputs, output)
	}

	def withNewInputs(newInputs: List[Int]) = new IntcodeProgram(codes, instructionPointer, newInputs, Option.empty)

}