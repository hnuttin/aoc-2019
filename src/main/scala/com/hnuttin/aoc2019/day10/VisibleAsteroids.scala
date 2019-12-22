package com.hnuttin.aoc2019.day10

import com.hnuttin.aoc2019.common.Coordinate
import com.hnuttin.aoc2019.common.Coordinate.asCoord
import com.hnuttin.aoc2019.day10.Visibility.Visibility

object Visibility extends Enumeration {
	type Visibility = Value
	val UNKNOWN, VISIBLE, INVISIBLE, STATION = Value
}

class VisibleAsteroids(val map: Array[Array[Visibility]]) {

	def anyVisible: Boolean = visibleCoordinates.nonEmpty

	def getVisibleInQ1(coordinate: Coordinate): List[Coordinate] = {
		visibleCoordinates.filter(c => c.x >= coordinate.x && c.y < coordinate.y)
	}

	def getVisibleInQ2(coordinate: Coordinate): List[Coordinate] = {
		visibleCoordinates.filter(c => c.x > coordinate.x && c.y >= coordinate.y)
	}

	def getVisibleInQ3(coordinate: Coordinate): List[Coordinate] = {
		visibleCoordinates.filter(c => c.x <= coordinate.x && c.y > coordinate.y)
	}

	def getVisibleInQ4(coordinate: Coordinate): List[Coordinate] = {
		visibleCoordinates.filter(c => c.x < coordinate.x && c.y <= coordinate.y)
	}

	def visibleCoordinates: List[Coordinate] = {
		map.zipWithIndex
				.flatMap(line => line._1.zipWithIndex.filter(v => v._1 == Visibility.VISIBLE).map(v => asCoord(v._2, line._2)))
				.toList
	}

	def isUnknown(coord: Coordinate): Boolean = map(coord.y)(coord.x) == Visibility.UNKNOWN

	def markVisible(coord: Coordinate): Unit = map(coord.y)(coord.x) = Visibility.VISIBLE

	def markStation(coord: Coordinate): Unit = map(coord.y)(coord.x) = Visibility.STATION

	def markInvisibleAbove(coord: Coordinate): Unit = List.range(0, coord.y).foreach(y => markInvisible(asCoord(coord.x, y)))

	def markInvisible(coord: Coordinate): Unit = map(coord.y)(coord.x) = Visibility.INVISIBLE

	def markInvisibleBelow(coord: Coordinate): Unit = List.range(coord.y + 1, map.length).foreach(y => markInvisible(asCoord(coord.x, y)))

	def markInvisibleRightOff(coord: Coordinate): Unit = List.range(coord.x + 1, map.head.length).foreach(x => markInvisible(asCoord(x, coord.y)))

	def markInvisibleLeftOff(coord: Coordinate): Unit = List.range(0, coord.x).foreach(x => markInvisible(asCoord(x, coord.y)))

	def isWithinBounds(coord: Coordinate): Boolean = coord.x >= 0 && coord.x < map.head.length && coord.y >= 0 && coord.y < map.length

	def countVisible(): Int = map.flatten.count(v => v == Visibility.VISIBLE)

	def get(x: Int, y: Int): Visibility = map(y)(x)

	def printMap(): Unit = map.foreach(row => printRow(row))

	private def printRow(row: Array[Visibility]): Unit = {
		println
		row.map(v => if (v == Visibility.VISIBLE) '#' else if (v == Visibility.UNKNOWN) '.' else if (v == Visibility.STATION) 'X' else '/')
				.foreach(char => print(char))
	}
}

object VisibleAsteroids {
	def initialize(width: Int, height: Int): VisibleAsteroids = {
		new VisibleAsteroids(Array.fill(height)(Array.fill(width)(Visibility.UNKNOWN)))
	}
}