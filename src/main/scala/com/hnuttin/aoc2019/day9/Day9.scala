package com.hnuttin.aoc2019.day9

import com.hnuttin.aoc2019.day5.intcode.IntcodeProgram

object Day9 extends App {
	val program: IntcodeProgram = IntcodeProgram.fromIntcode(Input.intcode)
	program.executeUntilHalted(List(2))
	println("BOOST keycode: " + program.outputs)
}
