package com.hnuttin.aoc2019.day5

object Day5 extends App {

	println("Output: " + new IntcodeProgram(Input.intcode, List(1)).executeUntilHalted())

}
