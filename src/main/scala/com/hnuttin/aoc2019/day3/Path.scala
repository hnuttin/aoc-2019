package com.hnuttin.aoc2019.day3

import java.lang.Integer.parseInt

import com.hnuttin.aoc2019.common.Direction.Direction
import com.hnuttin.aoc2019.common.{Coordinate, Direction}

class Path(val coords: List[Coordinate]) {

	def intersections(otherPath: Path): List[Coordinate] = {
		coords.intersect(otherPath.coords)
	}

	def stepsToCoord(intersection: Coordinate): Int = {
		stepsToCoordInner(intersection, coords, 1)
	}

	@scala.annotation.tailrec
	private def stepsToCoordInner(intersection: Coordinate, coordsLeft: List[Coordinate], steps: Int): Int = {
		if (coordsLeft.head.equals(intersection)) steps else stepsToCoordInner(intersection, coordsLeft.tail, steps + 1)
	}

}

object Path {
	def parse(start: Coordinate, input: String): Path = {
		val directions = input.split(",")
				.map(input => Movement.parse(input))
				.flatMap(movement => List.fill(movement.steps)(movement.direction))
				.toList
		new Path(directionsToCoords(start, directions).tail)
	}

	private def directionsToCoords(position: Coordinate, directions: List[Direction]): List[Coordinate] = {
		directionsToCoordsInner(Nil, position, directions)
	}

	@scala.annotation.tailrec
	private def directionsToCoordsInner(accum: List[Coordinate], position: Coordinate, directions: List[Direction]): List[Coordinate] = {
		directions match {
			case head :: rest => directionsToCoordsInner(position :: accum, position.move(head), rest)
			case Nil => (position :: accum).reverse
		}
	}
}

class Movement(val direction: Direction, val steps: Int) {

}

object Movement {
	def parse(input: String): Movement = {
		new Movement(Direction.withName(input.substring(0, 1)), parseInt(input.substring(1)))
	}
}
