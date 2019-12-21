package com.hnuttin.aoc2019.day3

import com.hnuttin.aoc2019.common.Coordinate
import org.scalatest.FunSuite

class PathTest extends FunSuite {

	test("R2,U3,L1,D2") {
		val path = Path.parse(new Coordinate(1, 1), "R2,U3,L1,D2")
		assert(path.coords.equals(List(
			new Coordinate(2, 1),
			new Coordinate(3, 1),
			new Coordinate(3, 2),
			new Coordinate(3, 3),
			new Coordinate(3, 4),
			new Coordinate(2, 4),
			new Coordinate(2, 3),
			new Coordinate(2, 2))))
	}

	test("U2,R1,D3,L2") {
		val path = Path.parse(new Coordinate(1, 1), "U2,R1,D3,L2")
		assert(path.coords.equals(List(
			new Coordinate(1, 2),
			new Coordinate(1, 3),
			new Coordinate(2, 3),
			new Coordinate(2, 2),
			new Coordinate(2, 1),
			new Coordinate(2, 0),
			new Coordinate(1, 0),
			new Coordinate(0, 0))))
	}

}
