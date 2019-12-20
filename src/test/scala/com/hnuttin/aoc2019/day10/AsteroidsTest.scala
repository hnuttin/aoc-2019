package com.hnuttin.aoc2019.day10

import org.scalatest.FunSuite

class AsteroidsTest extends FunSuite {

	test("parse example1") {
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

	test("detectOtherAsteroids example1") {
		val asteroids = Asteroids.parse(".#..#\n.....\n#####\n....#\n...##")
		assert(asteroids.asteroids.head.detectOtherAsteroids(asteroids, 5, 5) == 7)
		assert(asteroids.asteroids(1).detectOtherAsteroids(asteroids, 5, 5) == 7)
		assert(asteroids.asteroids(2).detectOtherAsteroids(asteroids, 5, 5) == 6)
		assert(asteroids.asteroids(3).detectOtherAsteroids(asteroids, 5, 5) == 7)
		assert(asteroids.asteroids(4).detectOtherAsteroids(asteroids, 5, 5) == 7)
		assert(asteroids.asteroids(5).detectOtherAsteroids(asteroids, 5, 5) == 7)
		assert(asteroids.asteroids(6).detectOtherAsteroids(asteroids, 5, 5) == 5)
		assert(asteroids.asteroids(7).detectOtherAsteroids(asteroids, 5, 5) == 7)
		assert(asteroids.asteroids(8).detectOtherAsteroids(asteroids, 5, 5) == 8)
		assert(asteroids.asteroids(9).detectOtherAsteroids(asteroids, 5, 5) == 7)
	}

	test("searchMonitoringAsteroid example1") {
		val moitoring = Asteroids.parse(".#..#\n.....\n#####\n....#\n...##")
				.searchMonitoringAsteroid(5, 5)
		assert(moitoring._1.x == 3)
		assert(moitoring._1.y == 4)
		assert(moitoring._2 == 8)
	}

	test("searchMonitoringAsteroid example2") {
		val moitoring = Asteroids.parse("......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####")
				.searchMonitoringAsteroid(10, 10)
		assert(moitoring._1.x == 5)
		assert(moitoring._1.y == 8)
		assert(moitoring._2 == 33)
	}

	test("searchMonitoringAsteroid example3") {
		val moitoring = Asteroids.parse("#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.")
				.searchMonitoringAsteroid(10, 10)
		assert(moitoring._1.x == 1)
		assert(moitoring._1.y == 2)
		assert(moitoring._2 == 35)
	}

	test("searchMonitoringAsteroid example4") {
		val moitoring = Asteroids.parse(".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..")
				.searchMonitoringAsteroid(10, 10)
		assert(moitoring._1.x == 6)
		assert(moitoring._1.y == 3)
		assert(moitoring._2 == 41)
	}

	test("searchMonitoringAsteroid example5") {
		val moitoring = Asteroids.parse(".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##")
				.searchMonitoringAsteroid(20, 20)
		assert(moitoring._1.x == 11)
		assert(moitoring._1.y == 13)
		assert(moitoring._2 == 210)
	}

}
