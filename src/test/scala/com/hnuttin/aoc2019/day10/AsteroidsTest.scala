package com.hnuttin.aoc2019.day10

import org.scalatest.FunSuite

class AsteroidsTest extends FunSuite {

	test("parse") {
		val asteroids = Asteroids.parse(".#..#\n.....\n#####\n....#\n...##")
		assert(asteroids.asteroids.length == 10)
		assert(asteroids.existsAt(1, 0))
		assert(asteroids.existsAt(4, 0))
		assert(asteroids.existsAt(0, 2))
		assert(asteroids.existsAt(1, 2))
		assert(asteroids.existsAt(2, 2))
		assert(asteroids.existsAt(3, 2))
		assert(asteroids.existsAt(4, 2))
		assert(asteroids.existsAt(4, 3))
		assert(asteroids.existsAt(3, 4))
		assert(asteroids.existsAt(4, 4))
	}

}
