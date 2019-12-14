package com.hnuttin.aoc2019.day6

import org.scalatest.FunSuite

class OrbitCalculatorTest extends FunSuite {

	test("example") {
		val totalOrbits = OrbitCalculator.calculateTotalOrbits(List("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"))
		assert(totalOrbits == 42)
	}

}
