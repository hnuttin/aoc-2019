package com.hnuttin.aoc2019.day5

import com.hnuttin.aoc2019.day5.ParameterMode.ParameterMode

abstract class Opcode {
	def executeUntilHalted(program: IntcodeProgram, inputs: List[Int]): Int = {
		val newProgram = operate(program, inputs)
		if (newProgram.isDefined) newProgram.get.executeUntilHalted(newInputs(inputs)) else program.output.getOrElse(0)
	}

	protected def newInputs(inputs: List[Int]): List[Int] = inputs

	def executeUntilOutputOrHalted(program: IntcodeProgram, inputs: List[Int]): (Option[Int], Option[IntcodeProgram]) = {
		val newProgram = operate(program, inputs)
		if (newProgram.isDefined) {
			if (newProgram.get.output.isDefined) {
				Tuple2(newProgram.get.output, newProgram)
			} else {
				newProgram.get.executeUntilOutputOrHalted(newInputs(inputs))
			}
		} else {
			Tuple2(program.output, Option.empty)
		}
	}

	protected def operate(program: IntcodeProgram, inputs: List[Int]): Option[IntcodeProgram]
}

private object ParameterMode extends Enumeration {
	type ParameterMode = Value
	val POSITION, IMMEDIATE = Value
}

private abstract class OpcodeWithParameterModes(modes: Array[ParameterMode]) extends Opcode {
	protected def getParameterValue(program: IntcodeProgram, paramPosition: Int): Int = {
		program.getParameter(paramPosition, getModeForParamPosition(paramPosition))
	}

	private def getModeForParamPosition(paramPosition: Int): ParameterMode = {
		if (paramPosition <= modes.length) modes(paramPosition - 1) else ParameterMode.POSITION
	}
}

private abstract class MathOpcode(modes: Array[ParameterMode]) extends OpcodeWithParameterModes(modes) {
	def mathOperate(program: IntcodeProgram, operator: (Int, Int) => Int): Option[IntcodeProgram] = {
		val value1 = getParameterValue(program, 1)
		val value2 = getParameterValue(program, 2)
		val calculatedValue = operator.apply(value1, value2)
		val positionToReplace = program.getParameter(3, ParameterMode.IMMEDIATE)
		Option(program.transformAndIncrementPointer(positionToReplace, calculatedValue, 4))
	}
}

private class AddOpcode(modes: Array[ParameterMode]) extends MathOpcode(modes) {
	override def operate(program: IntcodeProgram, inputs: List[Int]): Option[IntcodeProgram] = {
		mathOperate(program, (x, y) => x + y)
	}
}

private class MultiplyOpcode(modes: Array[ParameterMode]) extends MathOpcode(modes) {
	override def operate(program: IntcodeProgram, inputs: List[Int]): Option[IntcodeProgram] = {
		mathOperate(program, (x, y) => x * y)
	}
}

private class LessThanOpcode(modes: Array[ParameterMode]) extends MathOpcode(modes) {
	override def operate(program: IntcodeProgram, inputs: List[Int]): Option[IntcodeProgram] = {
		mathOperate(program, (x, y) => if (x < y) 1 else 0)
	}
}

private class EqualsOpcode(modes: Array[ParameterMode]) extends MathOpcode(modes) {
	override def operate(program: IntcodeProgram, inputs: List[Int]): Option[IntcodeProgram] = {
		mathOperate(program, (x, y) => if (x == y) 1 else 0)
	}
}

abstract private class JumpOpcode(modes: Array[ParameterMode]) extends OpcodeWithParameterModes(modes) {
	override def operate(program: IntcodeProgram, inputs: List[Int]): Option[IntcodeProgram] = {
		val value = getParameterValue(program, 1)
		if (shouldJump(value)) {
			val newPointerPosition = getParameterValue(program, 2)
			Option(program.setPointer(newPointerPosition))
		} else {
			Option(program.incrementPointer(3))
		}
	}

	protected def shouldJump(value: Int): Boolean
}

private class JumpIfTrueOpcode(modes: Array[ParameterMode]) extends JumpOpcode(modes) {
	override protected def shouldJump(value: Int): Boolean = value != 0
}

private class JumpIfFalseOpcode(modes: Array[ParameterMode]) extends JumpOpcode(modes) {
	override protected def shouldJump(value: Int): Boolean = value == 0
}

private class InputOpCode extends Opcode {
	override def newInputs(inputs: List[Int]): List[Int] = inputs.tail

	override def operate(program: IntcodeProgram, inputs: List[Int]): Option[IntcodeProgram] = {
		val positionToReplace = program.getParameter(1, ParameterMode.IMMEDIATE)
		Option(program.transformWithInputAndIncrementPointer(inputs.head, positionToReplace, 2))
	}
}

private class OutputOpCode(modes: Array[ParameterMode]) extends OpcodeWithParameterModes(modes) {
	override def operate(program: IntcodeProgram, inputs: List[Int]): Option[IntcodeProgram] = {
		val output = getParameterValue(program, 1)
		Option(program.incrementPointerAndAddOutput(2, output))
	}
}

private class HaltingOpCode extends Opcode {
	override def operate(program: IntcodeProgram, inputs: List[Int]): Option[IntcodeProgram] = {
		Option.empty
	}
}

object Opcode {
	def parse(code: Int): Opcode = {
		val codeValue = String.valueOf(code)
		if (codeValue.length > 2) {
			val modesAndOp = codeValue.splitAt(codeValue.length - 2)
			val modes = convertToParamModex(modesAndOp._1)
			constructOpCode(Integer.parseInt(modesAndOp._2), modes)
		} else {
			constructOpCode(Integer.parseInt(codeValue), Array.empty)
		}
	}

	private def convertToParamModex(modes: String): Array[ParameterMode] = {
		modes.toCharArray.reverse.map {
			case '0' => ParameterMode.POSITION
			case '1' => ParameterMode.IMMEDIATE
		}
	}

	private def constructOpCode(op: Int, modes: Array[ParameterMode.ParameterMode]): Opcode = {
		op match {
			case 1 => new AddOpcode(modes)
			case 2 => new MultiplyOpcode(modes)
			case 3 => new InputOpCode()
			case 4 => new OutputOpCode(modes)
			case 5 => new JumpIfTrueOpcode(modes)
			case 6 => new JumpIfFalseOpcode(modes)
			case 7 => new LessThanOpcode(modes)
			case 8 => new EqualsOpcode(modes)
			case 99 => new HaltingOpCode()
		}
	}
}
