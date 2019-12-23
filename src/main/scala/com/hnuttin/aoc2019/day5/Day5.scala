package com.hnuttin.aoc2019.day5

import com.hnuttin.aoc2019.day5.intcode.IntcodeProgram

object Day5 extends App {

	println("Output: " + IntcodeProgram.fromIntcode(Input.intcode).execute(() => 1, _ => ()))

}
