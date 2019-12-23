package com.hnuttin.aoc2019.day13

import com.hnuttin.aoc2019.day13.Tile.Tile
import com.hnuttin.aoc2019.day5.intcode.IntcodeProgram

object Tile extends Enumeration {
	type Tile = Value
	val EMPTY, WALL, BLOCK, PADDLE, BALL = Value
}

class Arcade(val program: IntcodeProgram) {

	private val tiles = new Tiles()

	def runGame(): Tiles = {
		runGameUntilHalted()
	}

	@scala.annotation.tailrec
	private def runGameUntilHalted(): Tiles = {
		if (program.halted) {
			tiles
		} else {
			program.executeUntilOutputOrHalted()
			program.executeUntilOutputOrHalted()
			program.executeUntilOutputOrHalted()
			tiles.setTile(xOutput, yOutput, tileOutput)
			runGameUntilHalted()
		}
	}

	private def xOutput: Int = program.outputs(program.outputs.length - 3).intValue

	private def yOutput: Int = program.outputs(program.outputs.length - 2).intValue

	private def tileOutput: Tile = program.outputs.last.intValue match {
		case 0 => Tile.EMPTY
		case 1 => Tile.WALL
		case 2 => Tile.BLOCK
		case 3 => Tile.PADDLE
		case 4 => Tile.BALL
	}

}
