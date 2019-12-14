package com.hnuttin.aoc2019.day6

import scala.collection.immutable.HashMap

object OrbitCalculator {

	def calculateTotalOrbits(input: List[String]): Int = {
		val planetOrbitMap = toPlanetOrbitMap(new HashMap[String, String](), input)
		planetOrbitMap.keys.toList.map(planet => calculateOrbits(planetOrbitMap, planet)).sum
	}

	@scala.annotation.tailrec
	private def toPlanetOrbitMap(orbitMap: Map[String, String], input: List[String]): Map[String, String] = {
		input match {
			case planetOrbit :: rest => toPlanetOrbitMap(addPlanetOrbit(orbitMap, planetOrbit), rest)
			case Nil => orbitMap
		}
	}

	private def addPlanetOrbit(accum: Map[String, String], planetOrbit: String): Map[String, String] = {
		val parsed = planetOrbit.split("\\)")
		val orbitingAround = parsed(0)
		val planet = parsed(1)
		accum + (planet -> orbitingAround)
	}

	private def calculateOrbits(orbitMap: Map[String, String], planet: String): Int = {
		calculateOrbitsAccum(1, orbitMap, planet)
	}

	@scala.annotation.tailrec
	private def calculateOrbitsAccum(orbits: Int, orbitMap: Map[String, String], planet: String): Int = {
		val orbitingAround = orbitMap(planet)
		orbitingAround match {
			case "COM" => orbits
			case _ => calculateOrbitsAccum(orbits + 1, orbitMap, orbitingAround)
		}
	}

}
