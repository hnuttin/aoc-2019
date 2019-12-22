package com.hnuttin.aoc2019.day10

import com.hnuttin.aoc2019.common.Coordinate.asCoord
import org.scalatest.FunSuite

class VisibleAsteroidsTest extends FunSuite {

	test("markInvisibleAbove") {
		val visibleAsteroids = VisibleAsteroids.initialize(3, 3)
		visibleAsteroids.markInvisibleAbove(asCoord(1, 2))
		assert(visibleAsteroids.get(0, 0) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(1, 0) == Visibility.INVISIBLE)
		assert(visibleAsteroids.get(2, 0) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(0, 1) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(1, 1) == Visibility.INVISIBLE)
		assert(visibleAsteroids.get(2, 1) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(0, 2) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(1, 2) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(2, 2) == Visibility.UNKNOWN)
	}

	test("markInvisibleBelow") {
		val visibleAsteroids = VisibleAsteroids.initialize(3, 3)
		visibleAsteroids.markInvisibleBelow(asCoord(2, 0))
		assert(visibleAsteroids.get(0, 0) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(1, 0) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(2, 0) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(0, 1) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(1, 1) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(2, 1) == Visibility.INVISIBLE)
		assert(visibleAsteroids.get(0, 2) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(1, 2) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(2, 2) == Visibility.INVISIBLE)
	}

	test("markInvisibleLeftOff") {
		val visibleAsteroids = VisibleAsteroids.initialize(3, 3)
		visibleAsteroids.markInvisibleLeftOff(asCoord(2, 1))
		assert(visibleAsteroids.get(0, 0) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(1, 0) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(2, 0) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(0, 1) == Visibility.INVISIBLE)
		assert(visibleAsteroids.get(1, 1) == Visibility.INVISIBLE)
		assert(visibleAsteroids.get(2, 1) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(0, 2) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(1, 2) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(2, 2) == Visibility.UNKNOWN)
	}

	test("markInvisibleRightOff") {
		val visibleAsteroids = VisibleAsteroids.initialize(3, 3)
		visibleAsteroids.markInvisibleRightOff(asCoord(0, 2))
		assert(visibleAsteroids.get(0, 0) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(1, 0) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(2, 0) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(0, 1) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(1, 1) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(2, 1) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(0, 2) == Visibility.UNKNOWN)
		assert(visibleAsteroids.get(1, 2) == Visibility.INVISIBLE)
		assert(visibleAsteroids.get(2, 2) == Visibility.INVISIBLE)
	}

}
