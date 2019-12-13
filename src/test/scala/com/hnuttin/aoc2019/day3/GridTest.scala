package com.hnuttin.aoc2019.day3

import org.scalatest.FunSuite

class GridTest extends FunSuite {

	test("manhattenDistanceClosestIntersection example1") {
		val port = new Coordinate(1, 1)
		val grid = Grid.parse(port, "R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")
		assert(grid.manhattenDistanceClosestIntersection === 159)
	}

	test("manhattenDistanceClosestIntersection example2") {
		val port = new Coordinate(1, 1)
		val grid = Grid.parse(port, "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
		assert(grid.manhattenDistanceClosestIntersection === 135)
	}

	test("fewestStepsToInteraction example1") {
		val port = new Coordinate(1, 1)
		val grid = Grid.parse(port, "R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")
		assert(grid.fewestStepsToInteraction === 610)
	}

	test("fewestStepsToInteraction example2") {
		val port = new Coordinate(1, 1)
		val grid = Grid.parse(port, "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
		assert(grid.fewestStepsToInteraction === 410)
	}

}
