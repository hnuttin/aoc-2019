package com.hnuttin.aoc2019.day13

import com.hnuttin.aoc2019.day5.intcode.IntcodeProgram
import org.scalatest.FunSuite

class ArcadeTest extends FunSuite {

	test("day13") {
		val tiles = new Arcade(IntcodeProgram.fromIntcode(Input.intcode)).runGame()
		tiles.draw()
	}

}
