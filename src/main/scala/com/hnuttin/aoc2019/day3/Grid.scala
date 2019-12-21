package com.hnuttin.aoc2019.day3

import com.hnuttin.aoc2019.common.Coordinate

class Grid(val port: Coordinate, val wire1: Path, val wire2: Path) {

	def manhattenDistanceClosestIntersection: Int = {
		wire1.intersections(wire2).map(calculateManhattenDistance).min
	}

	def fewestStepsToInteraction: Int = {
		wire1.intersections(wire2).map(calculateSteps).min
	}

	private def calculateManhattenDistance(intersection: Coordinate): Int = {
		Math.abs(port.x - intersection.x) + Math.abs(port.y - intersection.y)
	}

	private def calculateSteps(intersection: Coordinate): Int = {
		wire1.stepsToCoord(intersection) + wire2.stepsToCoord(intersection)
	}

}

object Grid {

	def parse(port: Coordinate, wire1: String, wire2: String): Grid = {
		new Grid(port, Path.parse(port, wire1), Path.parse(port, wire2))
	}
}
