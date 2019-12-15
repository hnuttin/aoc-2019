package com.hnuttin.aoc2019.day5

import org.scalatest.FunSuite

class IntcodeProgramTest extends FunSuite {

	test("3,0,4,0,99") {
		assertProgram(List(3, 0, 4, 0, 99), 50, 50)
	}

	test("is equal to 8 with position mode") {
		assertProgram(List(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8), 8, 1)
	}

	test("is not equal to 8 with position mode") {
		assertProgram(List(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8), 10, 0)
	}

	test("is less than 8 with position mode") {
		assertProgram(List(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8), 6, 1)
	}

	test("is not less than 8 with position mode") {
		assertProgram(List(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8), 8, 0)
	}

	test("is equal to 8 with immediate mode") {
		assertProgram(List(3, 3, 1108, -1, 8, 3, 4, 3, 99), 8, 1)
	}

	test("is not equal to 8 with immediate mode") {
		assertProgram(List(3, 3, 1108, -1, 8, 3, 4, 3, 99), 10, 0)
	}

	test("is less than 8 with immediate mode") {
		assertProgram(List(3, 3, 1107, -1, 8, 3, 4, 3, 99), 6, 1)
	}

	test("is not less than 8 with immediate mode") {
		assertProgram(List(3, 3, 1107, -1, 8, 3, 4, 3, 99), 8, 0)
	}

	test("jump example with position mode: is zero") {
		assertProgram(List(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9), 0, 0)
	}

	test("jump example with position mode: is not zero") {
		assertProgram(List(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9), 7, 1)
	}

	test("jump example with immediate mode: is zero") {
		assertProgram(List(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1), 0, 0)
	}

	test("jump example with immediate mode: is not zero") {
		assertProgram(List(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1), 7, 1)
	}

	test("large exmple: less then 8") {
		assertProgram(List(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99), 7, 999)
	}

	test("large exmple: equal 8") {
		assertProgram(List(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99), 8, 1000)
	}

	test("large exmple: greater than 8") {
		assertProgram(List(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99), 9, 1001)
	}

	test("1002,4,3,4,33") {
		new IntcodeProgram(List(1002, 4, 3, 4, 33)).executeUntilHalted(List())
	}

	private def assertProgram(intcode: List[Int], input: Int, output: Int): Any = {
		val program = new IntcodeProgram(intcode)
		val output = program.executeUntilHalted(List(input))
		assert(output == output)
	}

}
