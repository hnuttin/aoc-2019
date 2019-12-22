package com.hnuttin.aoc2019.day12

import com.hnuttin.aoc2019.day12.Coordinate3D.asCoord3D

class Moons(var moons: List[Moon]) {

	def simulate(numberOfSteps: Int): Unit = {
		List.range(0, numberOfSteps).foreach(_ => step())
	}

	def simulateLoop(): Long = {
		val initialState = copyMoons(moons)
		val stepsForXLoop = stepsForAxisLoop(initialState, (coord: Coordinate3D) => coord.x)
		moons = copyMoons(initialState)
		val stepsForYLoop = stepsForAxisLoop(initialState, (coord: Coordinate3D) => coord.y)
		moons = copyMoons(initialState)
		val stepsForZLoop = stepsForAxisLoop(initialState, (coord: Coordinate3D) => coord.z)
		lcm(List(stepsForXLoop, stepsForYLoop, stepsForZLoop))
	}

	private def copyMoons(moons: List[Moon]): List[Moon] = {
		moons.map(moon => moon.copy())
	}

	def lcm(list: Seq[Long]): Long = list.foldLeft(1: Long) {
		(a, b) =>
			b * a /
					LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
	}

	private def stepsForAxisLoop(initialState: List[Moon], axisFn: Coordinate3D => Int) = {
		var steps = 1L
		do {
			steps += 1
			step()
		} while (!areEqualForAxis(initialState, axisFn))
		steps
	}

	private def areEqualForAxis(initialState: List[Moon], axisFn: Coordinate3D => Int): Boolean = {
		moons.zip(initialState).forall(p => axisFn(p._1.position) == axisFn(p._2.position))
	}

	private def step(): Unit = {
		applyGravity()
		applyVelocity()
	}

	private def applyGravity(): Unit = {
		moons.foreach(moon => applyGravity(moon))
	}

	private def applyGravity(moon: Moon): Unit = {
		moons.filter(otherMoon => otherMoon.position != moon.position).foreach(otherMoon => applyGravity(moon, otherMoon))
	}

	private def applyGravity(moon: Moon, otherMoon: Moon): Unit = {
		moon.modifyVelocity(asCoord3D(
			calculateModifier(moon.position.x, otherMoon.position.x),
			calculateModifier(moon.position.y, otherMoon.position.y),
			calculateModifier(moon.position.z, otherMoon.position.z)))
	}

	private def calculateModifier(value1: Int, value2: Int): Int = {
		val modifier = (value1 - value2) * -1
		if (modifier == 0) 0 else modifier / (Math.abs(modifier))
	}

	private def applyVelocity(): Unit = {
		moons.foreach(moon => moon.applyVelocity())
	}

	def totalEnergy: Int = moons.map(moon => moon.totalEnergy).sum

}

object Moons {
	def parse(input: String): Moons = {
		new Moons(input.split("\n").map(moon => parseMoon(moon)).toList)
	}

	private def parseMoon(moon: String): Moon = {
		val coords = moon.substring(1, moon.length - 1).split(", ")
		new Moon(
			asCoord3D(parseCoord(coords.head), parseCoord(coords(1)), parseCoord(coords(2))),
			asCoord3D(0, 0, 0))
	}

	private def parseCoord(coord: String): Int = Integer.parseInt(coord.split("=").last)
}