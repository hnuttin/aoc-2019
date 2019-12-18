package com.hnuttin.aoc2019.day9

import com.hnuttin.aoc2019.day5.intcode.IntcodeProgram
import org.scalatest.FunSuite

class IntcodeProgramTest extends FunSuite {

	test("example1") {
		val program = IntcodeProgram.fromIntcode(List(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99))
		program.executeUntilHalted(List())
		assert(program.outputs == List(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99))
	}

	test("example2") {
		val program = IntcodeProgram.fromIntcode(List(1102, 34915192, 34915192, 7, 4, 7, 99, 0))
		program.executeUntilHalted(List())
		assert(program.outputs == List(1219070632396864L))
	}

	test("example3") {
		val program = IntcodeProgram.fromLongCode(List(104, 1125899906842624L, 99))
		program.executeUntilHalted(List())
		assert(program.outputs == List(1125899906842624L))
	}

	test("input with relative mode1 - should echo input") {
		val program = IntcodeProgram.fromIntcode(List(203, 3, 104, 0, 99))
		program.executeUntilHalted(List(5))
		assert(program.outputs == List(5))
	}

	test("input with relative mode2 - should echo input") {
		val program = IntcodeProgram.fromIntcode(List(109, 5, 203, 0, 104, 0, 99))
		program.executeUntilHalted(List(5))
		assert(program.outputs == List(5))
	}

	test("relative base op with relative mode") {
		val program = IntcodeProgram.fromIntcode(List(109, 5, 209, 5, 203, 0, 104, 15, 99))
		program.executeUntilHalted(List(5))
		assert(program.outputs == List(15))
	}

	test("day9") {
		val program = IntcodeProgram.fromIntcode(Input.intcode)
		program.executeUntilHalted(List(1))
		assert(program.outputs == List(2377080455L))
	}

}
