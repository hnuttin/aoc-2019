package com.hnuttin.aoc2019.day12

import com.hnuttin.aoc2019.day12.Coordinate3D.asCoord3D

class Coordinate3D(val x: Int, val y: Int, val z: Int) {

	def add(otherCoordinate3D: Coordinate3D): Coordinate3D = {
		asCoord3D(x + otherCoordinate3D.x, y + otherCoordinate3D.y, z + otherCoordinate3D.z)
	}

	override def equals(other: Any): Boolean = other match {
		case that: Coordinate3D =>
			(that canEqual this) &&
					x == that.x &&
					y == that.y &&
					z == that.z
		case _ => false
	}

	def canEqual(other: Any): Boolean = other.isInstanceOf[Coordinate3D]

	override def hashCode(): Int = {
		val state = Seq(x, y, z)
		state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
	}

	override def toString: String = s"($x, $y, $z)"
}

object Coordinate3D {
	def asCoord3D(x: Int, y: Int, z: Int): Coordinate3D = new Coordinate3D(x, y, z)
}

class Moon(var position: Coordinate3D, var velocity: Coordinate3D) {

	def applyVelocity(): Unit = {
		position = position.add(velocity)
	}

	def modifyVelocity(modifier: Coordinate3D): Unit = {
		velocity = velocity.add(modifier)
	}

	def totalEnergy: Int = potentialEnergy * kineticEnergy

	def potentialEnergy: Int = position.x.abs + position.y.abs + position.z.abs

	def kineticEnergy: Int = velocity.x.abs + velocity.y.abs + velocity.z.abs

	def copy(): Moon = new Moon(position, velocity)

	override def equals(other: Any): Boolean = other match {
		case that: Moon =>
			(that canEqual this) &&
					position == that.position &&
					velocity == that.velocity
		case _ => false
	}

	def canEqual(other: Any): Boolean = other.isInstanceOf[Moon]

	override def hashCode(): Int = {
		val state = Seq(position, velocity)
		state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
	}
}
