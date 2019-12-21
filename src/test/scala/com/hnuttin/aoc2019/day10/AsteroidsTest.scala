package com.hnuttin.aoc2019.day10

import com.hnuttin.aoc2019.common.Coordinate.asCoord
import org.scalatest.FunSuite

class AsteroidsTest extends FunSuite {

	test("parse example1") {
		val asteroids = Asteroids.parse(".#..#\n.....\n#####\n....#\n...##")
		assert(asteroids.asteroids.length == 10)
		assert(asteroids.existsAt(asCoord(1, 0)))
		assert(asteroids.existsAt(asCoord(4, 0)))
		assert(asteroids.existsAt(asCoord(0, 2)))
		assert(asteroids.existsAt(asCoord(1, 2)))
		assert(asteroids.existsAt(asCoord(2, 2)))
		assert(asteroids.existsAt(asCoord(3, 2)))
		assert(asteroids.existsAt(asCoord(4, 2)))
		assert(asteroids.existsAt(asCoord(4, 3)))
		assert(asteroids.existsAt(asCoord(3, 4)))
		assert(asteroids.existsAt(asCoord(4, 4)))
	}

	test("detectOtherAsteroids example1") {
		val asteroids = Asteroids.parse(".#..#\n.....\n#####\n....#\n...##")
		assert(asteroids.asteroids.head.constructVisibilityMap(asteroids, 5, 5).countVisible() == 7)
		assert(asteroids.asteroids(1).constructVisibilityMap(asteroids, 5, 5).countVisible() == 7)
		assert(asteroids.asteroids(2).constructVisibilityMap(asteroids, 5, 5).countVisible() == 6)
		assert(asteroids.asteroids(3).constructVisibilityMap(asteroids, 5, 5).countVisible() == 7)
		assert(asteroids.asteroids(4).constructVisibilityMap(asteroids, 5, 5).countVisible() == 7)
		assert(asteroids.asteroids(5).constructVisibilityMap(asteroids, 5, 5).countVisible() == 7)
		assert(asteroids.asteroids(6).constructVisibilityMap(asteroids, 5, 5).countVisible() == 5)
		assert(asteroids.asteroids(7).constructVisibilityMap(asteroids, 5, 5).countVisible() == 7)
		assert(asteroids.asteroids(8).constructVisibilityMap(asteroids, 5, 5).countVisible() == 8)
		assert(asteroids.asteroids(9).constructVisibilityMap(asteroids, 5, 5).countVisible() == 7)
	}

	test("searchMonitoringAsteroid example1") {
		val moitoring = Asteroids.parse(".#..#\n.....\n#####\n....#\n...##")
				.searchMonitoringAsteroid(5, 5)
		assert(moitoring._1.asteroidCoord.x == 3)
		assert(moitoring._1.asteroidCoord.y == 4)
		assert(moitoring._2 == 8)
	}

	test("searchMonitoringAsteroid example2") {
		val moitoring = Asteroids.parse("......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####")
				.searchMonitoringAsteroid(10, 10)
		assert(moitoring._1.asteroidCoord.x == 5)
		assert(moitoring._1.asteroidCoord.y == 8)
		assert(moitoring._2 == 33)
	}

	test("searchMonitoringAsteroid example3") {
		val moitoring = Asteroids.parse("#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.")
				.searchMonitoringAsteroid(10, 10)
		assert(moitoring._1.asteroidCoord.x == 1)
		assert(moitoring._1.asteroidCoord.y == 2)
		assert(moitoring._2 == 35)
	}

	test("searchMonitoringAsteroid example4") {
		val moitoring = Asteroids.parse(".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..")
				.searchMonitoringAsteroid(10, 10)
		assert(moitoring._1.asteroidCoord.x == 6)
		assert(moitoring._1.asteroidCoord.y == 3)
		assert(moitoring._2 == 41)
	}

	test("searchMonitoringAsteroid example5") {
		val asteroids = Asteroids.parse(".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##")
		asteroids.asteroids.find(a => a.asteroidCoord.equals(asCoord(11, 13))).foreach(a => a.constructVisibilityMap(asteroids, 20, 20).printMap())
		val moitoring = asteroids.searchMonitoringAsteroid(20, 20)
		assert(moitoring._1.asteroidCoord.x == 11)
		assert(moitoring._1.asteroidCoord.y == 13)
		assert(moitoring._2 == 210)
	}

	test("diagonal test from reddit") {
		val asteroids = Asteroids.parse("#.........\n..........\n....#.....\n......#...")
		val visibilityMap = asteroids.asteroids.head.constructVisibilityMap(asteroids, 10, 4)
		assert(visibilityMap.get(0, 0) == Visibility.STATION)
		assert(visibilityMap.get(4, 2) == Visibility.VISIBLE)
		assert(visibilityMap.get(6, 3) == Visibility.INVISIBLE)
	}
}
