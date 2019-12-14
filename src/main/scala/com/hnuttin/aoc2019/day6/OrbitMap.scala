package com.hnuttin.aoc2019.day6

import scala.collection.immutable.HashMap

class OrbitMap(val planetNodes: Map[String, List[String]]) {

	def calculateOrbits(from: String, to: String): Int = {
		calculateOrbitsAccum(-1, from, from, to).get
	}

	def calculateOrbitsAccum(orbits: Int, previous: String, from: String, to: String): Option[Int] = {
		val neighbours = planetNodes(from)
		if (neighbours.contains(to)) {
			Option(orbits)
		} else {
			val orbitCandidates = neighbours
					.filter(neighbour => !neighbour.equals(previous))
					.map(neighbour => calculateOrbitsAccum(orbits + 1, from, neighbour, to))
					.filter(orbits => orbits.isDefined)
			if (orbitCandidates.isEmpty) Option.empty else orbitCandidates.head
		}
	}
}

object OrbitMap {

	def parse(input: List[String]): OrbitMap = {
		val planetOrbits = toPlanetOrbitMap(HashMap(), input)
		new OrbitMap(buildPlanetNodesAccum(HashMap("COM" -> List()), planetOrbits, planetOrbits))
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

	@scala.annotation.tailrec
	private def buildPlanetNodesAccum(planetNodes: Map[String, List[String]], planetOrbitsLeft: Map[String, String], planetOrbits: Map[String, String]): Map[String, List[String]] = {
		if (planetOrbitsLeft.isEmpty) {
			planetNodes
		} else {
			buildPlanetNodesAccum(planetNodes + (planetOrbitsLeft.head._1 -> createPlanetNode(planetOrbitsLeft.head._1, planetOrbits)), planetOrbitsLeft.tail, planetOrbits)
		}
	}

	private def createPlanetNode(planet: String, planetOrbits: Map[String, String]): List[String] = {
		planetOrbits(planet) :: planetOrbits.filter(planetOrbit => planetOrbit._2.equals(planet)).keys.toList
	}
}

class PlanetOrbit(val planet: String, val orbiting: String) {

}