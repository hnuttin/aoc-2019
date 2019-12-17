package com.hnuttin.aoc2019.day9

import com.hnuttin.aoc2019.day5.intcode.IntcodeProgram

object Day9 extends App {
	println("BOOST keycode: " + IntcodeProgram.fromIntcode(Input.intcode).executeUntilHalted(List(2)))
}
