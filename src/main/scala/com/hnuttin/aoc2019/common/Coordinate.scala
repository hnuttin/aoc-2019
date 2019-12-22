package com.hnuttin.aoc2019.common

import com.hnuttin.aoc2019.common.Coordinate.asCoord
import com.hnuttin.aoc2019.common.Direction.Direction

object Direction extends Enumeration {
	type Direction = Value
	val U, D, L, R = Value

	def turnLeft(direction: Direction): Direction = {
		direction match {
			case Direction.U => Direction.L
			case Direction.D => Direction.R
			case Direction.L => Direction.D
			case Direction.R => Direction.U
		}
	}

	def turnRight(direction: Direction): Direction = {
		direction match {
			case Direction.U => Direction.R
			case Direction.D => Direction.L
			case Direction.L => Direction.U
			case Direction.R => Direction.D
		}
	}
}

class Coordinate(val x: Int, val y: Int) {

	def move(direction: Direction): Coordinate = {
		direction match {
			case Direction.U => asCoord(x, y + 1)
			case Direction.D => asCoord(x, y - 1)
			case Direction.L => asCoord(x - 1, y)
			case Direction.R => asCoord(x + 1, y)
		}
	}

	def add(otherCoordinate: Coordinate): Coordinate = asCoord(x + otherCoordinate.x, y + otherCoordinate.y)

	def minus(otherCoordinate: Coordinate): Coordinate = asCoord(x - otherCoordinate.x, y - otherCoordinate.y)

	def divide(divisor: Int): Coordinate = asCoord(x / divisor, y / divisor)

	def halve(): Coordinate = asCoord(x / 2, y / 2)

	def isEven: Boolean = (x % 2 == 0) && (y % 2 == 0)

	def isSameHeight(otherCoordinate: Coordinate): Boolean = y == otherCoordinate.y

	def isSameWidth(otherCoordinate: Coordinate): Boolean = x == otherCoordinate.x

	def isRightOff(otherCoordinate: Coordinate): Boolean = x > otherCoordinate.x

	def isBelow(otherCoordinate: Coordinate): Boolean = y > otherCoordinate.y

	override def equals(other: Any): Boolean = other match {
		case that: Coordinate =>
			(that canEqual this) &&
					x == that.x &&
					y == that.y
		case _ => false
	}

	def canEqual(other: Any): Boolean = other.isInstanceOf[Coordinate]

	override def hashCode(): Int = {
		val state = Seq(x, y)
		state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
	}

	override def toString: String = "(" + x + ", " + y + ")"
}

object Coordinate {
	def asCoord(x: Int, y: Int): Coordinate = new Coordinate(x, y)
}