package com.hnuttin.aoc2019.day3

import java.lang.Integer.parseInt

import com.hnuttin.aoc2019.day3.Direction.Direction

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

object Direction extends Enumeration {
	type Direction = Value
	val U, D, L, R = Value
}

class Movement(val direction: Direction, val steps: Int) {

}

object Movement {
	def parse(input: String): Movement = {
		new Movement(Direction.withName(input.substring(0, 1)), parseInt(input.substring(1)))
	}
}

class Coordinate(val x: Int, val y: Int) {

	def move(direction: Direction): Coordinate = {
		direction match {
			case Direction.U => new Coordinate(x, y + 1)
			case Direction.D => new Coordinate(x, y - 1)
			case Direction.L => new Coordinate(x - 1, y)
			case Direction.R => new Coordinate(x + 1, y)
		}
	}

	def canEqual(other: Any): Boolean = other.isInstanceOf[Coordinate]

	override def equals(other: Any): Boolean = other match {
		case that: Coordinate =>
			(that canEqual this) &&
					x == that.x &&
					y == that.y
		case _ => false
	}

	override def hashCode(): Int = {
		val state = Seq(x, y)
		state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
	}
}