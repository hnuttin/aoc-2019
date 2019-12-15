package com.hnuttin.aoc2019.day9

import com.hnuttin.aoc2019.day5.intcode.IntcodeProgram
import org.scalatest.FunSuite

class IntcodeProgramTest extends FunSuite {

	test("example1") {
		val output = IntcodeProgram.fromIntcode(List(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99)).executeUntilHalted(List())
		assert(output == 99)
	}

	test("example2") {
		val output = IntcodeProgram.fromIntcode(List(1102, 34915192, 34915192, 7, 4, 7, 99, 0)).executeUntilHalted(List())
		assert(output == 1219070632396864L)
	}

	test("example3") {
		val output = IntcodeProgram.fromLongCode(List(104, 1125899906842624L, 99)).executeUntilHalted(List())
		assert(output == 1125899906842624L)
	}

}
