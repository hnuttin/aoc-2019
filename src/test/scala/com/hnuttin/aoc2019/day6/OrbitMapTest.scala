package com.hnuttin.aoc2019.day6

import org.scalatest.FunSuite

class OrbitMapTest extends FunSuite {

	val orbitMap: OrbitMap = OrbitMap.parse(List("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"))

	test("example1") {
		assert(orbitMap.calculateOrbits("YOU", "SAN") == 4)
	}

	test("example2") {
		assert(orbitMap.calculateOrbits("L", "COM") == 5)
	}

	test("example3") {
		assert(orbitMap.calculateOrbits("H", "D") == 2)
	}

}
