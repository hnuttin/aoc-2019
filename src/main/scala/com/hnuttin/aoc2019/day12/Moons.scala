package com.hnuttin.aoc2019.day12

import com.hnuttin.aoc2019.day12.Coordinate3D.asCoord3D

class Moons(val moons: List[Moon]) {

	def simulate(numberOfSteps: Int): Unit = {
		List.range(0, numberOfSteps).foreach(_ => step())
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