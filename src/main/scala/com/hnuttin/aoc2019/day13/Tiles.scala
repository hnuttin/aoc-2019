package com.hnuttin.aoc2019.day13

import com.hnuttin.aoc2019.day13.Tile.Tile

import scala.collection.immutable.HashMap

class Tiles {

	private var tiles = HashMap[Int, Map[Int, Tile]]()

	def setTile(x: Int, y: Int, tile: Tile): Unit = {
		if (tiles.contains(y)) {
			val line: Map[Int, Tile] = tiles(y)
			tiles += y -> (line + (x -> tile))
		} else {
			tiles += y -> HashMap(x -> tile)
		}
	}

	def countBlocks(): Int = {
		tiles.values.flatMap(line => line.values).count(t => t == Tile.BLOCK)
	}

	def draw(): Unit = {
		List.range(minY, maxY + 1).reverse.foreach(y => drawLine(y))
	}

	private def drawLine(y: Int): Unit = {
		List.range(minX, maxX + 1).foreach(x => drawTile(tiles(y)(x)))
		println()
	}

	private def drawTile(tile: Tile): Unit = tile match {
		case Tile.BALL => print('O')
		case Tile.PADDLE => print('_')
		case Tile.BLOCK => print('X')
		case Tile.WALL => print('|')
		case Tile.EMPTY => print(' ')
	}

	private def minX: Int = {
		tiles.flatMap(line => line._2).keys.min
	}

	private def maxX: Int = {
		tiles.flatMap(line => line._2).keys.max
	}

	private def minY: Int = {
		tiles.keys.min
	}

	private def maxY: Int = {
		tiles.keys.max
	}

}
