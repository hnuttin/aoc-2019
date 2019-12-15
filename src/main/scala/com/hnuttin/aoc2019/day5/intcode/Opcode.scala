package com.hnuttin.aoc2019.day5.intcode

import com.hnuttin.aoc2019.day5.intcode.ParameterMode.ParameterMode

private object ParameterMode extends Enumeration {
	type ParameterMode = Value
	val POSITION, IMMEDIATE, RELATIVE = Value

	def parse(char: Char): ParameterMode.ParameterMode = char match {
		case '0' => ParameterMode.POSITION
		case '1' => ParameterMode.IMMEDIATE
		case '2' => ParameterMode.RELATIVE
	}
}

private class Parameters(modes: Array[ParameterMode]) {
	def get(program: IntcodeProgram, position: Long): Long = {
		program.getParameter(position, getModeForPosition(position))
	}

	private def getModeForPosition(position: Long): ParameterMode = {
		if (position <= modes.length) modes(position.intValue - 1) else ParameterMode.POSITION
	}
}

private[intcode] abstract class Opcode {
	def executeUntilHalted(program: IntcodeProgram, inputs: List[Long]): Long = {
		val newProgram = operate(program, inputs)
		if (newProgram.isDefined) newProgram.get.executeUntilHalted(newInputs(inputs)) else program.output.getOrElse(0)
	}

	def executeUntilOutputOrHalted(program: IntcodeProgram, inputs: List[Long]): (Option[Long], Option[IntcodeProgram]) = {
		val newProgram = operate(program, inputs)
		if (newProgram.isDefined) {
			if (newProgram.get.output.isDefined) {
				Tuple2(newProgram.get.output, Option(newProgram.get.clearOutput()))
			} else {
				newProgram.get.executeUntilOutputOrHalted(newInputs(inputs))
			}
		} else {
			Tuple2(program.output, Option.empty)
		}
	}

	protected def newInputs(inputs: List[Long]): List[Long] = inputs

	protected def operate(program: IntcodeProgram, inputs: List[Long]): Option[IntcodeProgram]
}

private abstract class MathOpcode(val parameters: Parameters) extends Opcode {
	def mathOperate(program: IntcodeProgram, operator: (Long, Long) => Long): Option[IntcodeProgram] = {
		val value1 = parameters.get(program, 1)
		val value2 = parameters.get(program, 2)
		val calculatedValue = operator.apply(value1, value2)
		val positionToReplace = program.getParameter(3, ParameterMode.IMMEDIATE)
		Option(program.transformAndIncrementPointer(positionToReplace, calculatedValue, 4))
	}
}

private class AddOpcode(parameters: Parameters) extends MathOpcode(parameters) {
	override def operate(program: IntcodeProgram, inputs: List[Long]): Option[IntcodeProgram] = {
		mathOperate(program, (x, y) => x + y)
	}
}

private class MultiplyOpcode(parameters: Parameters) extends MathOpcode(parameters) {
	override def operate(program: IntcodeProgram, inputs: List[Long]): Option[IntcodeProgram] = {
		mathOperate(program, (x, y) => x * y)
	}
}

private class LessThanOpcode(parameters: Parameters) extends MathOpcode(parameters) {
	override def operate(program: IntcodeProgram, inputs: List[Long]): Option[IntcodeProgram] = {
		mathOperate(program, (x, y) => if (x < y) 1 else 0)
	}
}

private class EqualsOpcode(parameters: Parameters) extends MathOpcode(parameters) {
	override def operate(program: IntcodeProgram, inputs: List[Long]): Option[IntcodeProgram] = {
		mathOperate(program, (x, y) => if (x == y) 1 else 0)
	}
}

abstract private class JumpOpcode(val parameters: Parameters) extends Opcode {
	override def operate(program: IntcodeProgram, inputs: List[Long]): Option[IntcodeProgram] = {
		val value = parameters.get(program, 1)
		if (shouldJump(value)) {
			val newPointerPosition = parameters.get(program, 2)
			Option(program.setPointer(newPointerPosition))
		} else {
			Option(program.incrementPointer(3))
		}
	}

	protected def shouldJump(value: Long): Boolean
}

private class JumpIfTrueOpcode(parameters: Parameters) extends JumpOpcode(parameters) {
	override protected def shouldJump(value: Long): Boolean = value != 0
}

private class JumpIfFalseOpcode(parameters: Parameters) extends JumpOpcode(parameters) {
	override protected def shouldJump(value: Long): Boolean = value == 0
}

private class RelativeBaseOpcode extends Opcode {
	override protected def operate(program: IntcodeProgram, inputs: List[Long]): Option[IntcodeProgram] = {
		val relativeBaseIncrement = program.getParameter(1, ParameterMode.IMMEDIATE)
		Option(program.incrementRelativeBaseAndIncrementPointer(relativeBaseIncrement, 2))
	}
}

private class InputOpCode extends Opcode {
	override def newInputs(inputs: List[Long]): List[Long] = inputs.tail

	override def operate(program: IntcodeProgram, inputs: List[Long]): Option[IntcodeProgram] = {
		val positionToReplace = program.getParameter(1, ParameterMode.IMMEDIATE)
		Option(program.transformAndIncrementPointer(positionToReplace, inputs.head, 2))
	}
}

private class OutputOpCode(val parameters: Parameters) extends Opcode {
	override def operate(program: IntcodeProgram, inputs: List[Long]): Option[IntcodeProgram] = {
		val output = parameters.get(program, 1)
		print(output)
		Option(program.incrementPointerAndSetOutput(2, output))
	}
}

private class HaltingOpCode extends Opcode {
	override def operate(program: IntcodeProgram, inputs: List[Long]): Option[IntcodeProgram] = {
		Option.empty
	}
}

private[intcode] object Opcode {
	def parse(code: Long): Opcode = {
		val codeValue = String.valueOf(code)
		if (codeValue.length > 2) {
			val modesAndOp = codeValue.splitAt(codeValue.length - 2)
			val modes = convertToParamModes(modesAndOp._1)
			constructOpCode(Integer.parseInt(modesAndOp._2), modes)
		} else {
			constructOpCode(Integer.parseInt(codeValue), Array.empty)
		}
	}

	private def convertToParamModes(modes: String): Array[ParameterMode] = {
		modes.toCharArray.reverse.map(ParameterMode.parse)
	}

	private def constructOpCode(op: Int, modes: Array[ParameterMode.ParameterMode]): Opcode = {
		op match {
			case 1 => new AddOpcode(new Parameters(modes))
			case 2 => new MultiplyOpcode(new Parameters(modes))
			case 3 => new InputOpCode()
			case 4 => new OutputOpCode(new Parameters(modes))
			case 5 => new JumpIfTrueOpcode(new Parameters(modes))
			case 6 => new JumpIfFalseOpcode(new Parameters(modes))
			case 7 => new LessThanOpcode(new Parameters(modes))
			case 8 => new EqualsOpcode(new Parameters(modes))
			case 9 => new RelativeBaseOpcode
			case 99 => new HaltingOpCode()
		}
	}
}
