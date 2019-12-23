package com.hnuttin.aoc2019.day13

import com.hnuttin.aoc2019.day5.intcode.IntcodeProgram

object Day13 extends App {

	private val tiles: Tiles = new Arcade(IntcodeProgram.fromIntcode(Input.intcode)).runGame()
	tiles.draw()
	println("Arcade blocks: " + tiles.countBlocks())

}
