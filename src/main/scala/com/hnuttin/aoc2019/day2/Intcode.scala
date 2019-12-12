package com.hnuttin.aoc2019.day2

class Intcode(val codes: List[Int]) {

	def execute(): Intcode = {
		executeAtPosition(0)
	}

	@scala.annotation.tailrec
	private def executeAtPosition(startPosition: Int): Intcode = {
		val opcode = codes(startPosition);
		val nextPosition = startPosition + 4
		opcode match {
			case 1 => transform(startPosition, (v1, v2) => v1 + v2).executeAtPosition(nextPosition)
			case 2 => transform(startPosition, (v1, v2) => v1 * v2).executeAtPosition(nextPosition)
			case 99 => this
			case _ => throw new IllegalArgumentException
		}
	}

	private def transform(position: Int, operator: (Int, Int) => Int): Intcode = {
		val value1 = codes(codes(position + 1))
		val value2 = codes(codes(position + 2))
		val positionToReplace = codes(position + 3)
		val newValue = operator.apply(value1, value2)
		new Intcode(codes.updated(positionToReplace, newValue))
	}

}
