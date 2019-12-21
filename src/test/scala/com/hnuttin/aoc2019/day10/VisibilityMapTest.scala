package com.hnuttin.aoc2019.day10

import com.hnuttin.aoc2019.common.Coordinate.asCoord
import org.scalatest.FunSuite

class VisibilityMapTest extends FunSuite {

	test("markInvisibleAbove") {
		val visibilityMap = VisibilityMap.initialize(3, 3)
		visibilityMap.markInvisibleAbove(asCoord(1, 2))
		assert(visibilityMap.get(0, 0) == Visibility.UNKNOWN)
		assert(visibilityMap.get(1, 0) == Visibility.INVISIBLE)
		assert(visibilityMap.get(2, 0) == Visibility.UNKNOWN)
		assert(visibilityMap.get(0, 1) == Visibility.UNKNOWN)
		assert(visibilityMap.get(1, 1) == Visibility.INVISIBLE)
		assert(visibilityMap.get(2, 1) == Visibility.UNKNOWN)
		assert(visibilityMap.get(0, 2) == Visibility.UNKNOWN)
		assert(visibilityMap.get(1, 2) == Visibility.UNKNOWN)
		assert(visibilityMap.get(2, 2) == Visibility.UNKNOWN)
	}

	test("markInvisibleBelow") {
		val visibilityMap = VisibilityMap.initialize(3, 3)
		visibilityMap.markInvisibleBelow(asCoord(2, 0))
		assert(visibilityMap.get(0, 0) == Visibility.UNKNOWN)
		assert(visibilityMap.get(1, 0) == Visibility.UNKNOWN)
		assert(visibilityMap.get(2, 0) == Visibility.UNKNOWN)
		assert(visibilityMap.get(0, 1) == Visibility.UNKNOWN)
		assert(visibilityMap.get(1, 1) == Visibility.UNKNOWN)
		assert(visibilityMap.get(2, 1) == Visibility.INVISIBLE)
		assert(visibilityMap.get(0, 2) == Visibility.UNKNOWN)
		assert(visibilityMap.get(1, 2) == Visibility.UNKNOWN)
		assert(visibilityMap.get(2, 2) == Visibility.INVISIBLE)
	}

	test("markInvisibleLeftOff") {
		val visibilityMap = VisibilityMap.initialize(3, 3)
		visibilityMap.markInvisibleLeftOff(asCoord(2, 1))
		assert(visibilityMap.get(0, 0) == Visibility.UNKNOWN)
		assert(visibilityMap.get(1, 0) == Visibility.UNKNOWN)
		assert(visibilityMap.get(2, 0) == Visibility.UNKNOWN)
		assert(visibilityMap.get(0, 1) == Visibility.INVISIBLE)
		assert(visibilityMap.get(1, 1) == Visibility.INVISIBLE)
		assert(visibilityMap.get(2, 1) == Visibility.UNKNOWN)
		assert(visibilityMap.get(0, 2) == Visibility.UNKNOWN)
		assert(visibilityMap.get(1, 2) == Visibility.UNKNOWN)
		assert(visibilityMap.get(2, 2) == Visibility.UNKNOWN)
	}

	test("markInvisibleRightOff") {
		val visibilityMap = VisibilityMap.initialize(3, 3)
		visibilityMap.markInvisibleRightOff(asCoord(0, 2))
		assert(visibilityMap.get(0, 0) == Visibility.UNKNOWN)
		assert(visibilityMap.get(1, 0) == Visibility.UNKNOWN)
		assert(visibilityMap.get(2, 0) == Visibility.UNKNOWN)
		assert(visibilityMap.get(0, 1) == Visibility.UNKNOWN)
		assert(visibilityMap.get(1, 1) == Visibility.UNKNOWN)
		assert(visibilityMap.get(2, 1) == Visibility.UNKNOWN)
		assert(visibilityMap.get(0, 2) == Visibility.UNKNOWN)
		assert(visibilityMap.get(1, 2) == Visibility.INVISIBLE)
		assert(visibilityMap.get(2, 2) == Visibility.INVISIBLE)
	}

}
