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
		assert(asteroids.asteroids.head.getVisibleAsteroids(asteroids).countVisible() == 7)
		assert(asteroids.asteroids(1).getVisibleAsteroids(asteroids).countVisible() == 7)
		assert(asteroids.asteroids(2).getVisibleAsteroids(asteroids).countVisible() == 6)
		assert(asteroids.asteroids(3).getVisibleAsteroids(asteroids).countVisible() == 7)
		assert(asteroids.asteroids(4).getVisibleAsteroids(asteroids).countVisible() == 7)
		assert(asteroids.asteroids(5).getVisibleAsteroids(asteroids).countVisible() == 7)
		assert(asteroids.asteroids(6).getVisibleAsteroids(asteroids).countVisible() == 5)
		assert(asteroids.asteroids(7).getVisibleAsteroids(asteroids).countVisible() == 7)
		assert(asteroids.asteroids(8).getVisibleAsteroids(asteroids).countVisible() == 8)
		assert(asteroids.asteroids(9).getVisibleAsteroids(asteroids).countVisible() == 7)
	}

	test("searchMonitoringAsteroid example1") {
		val moitoring = Asteroids.parse(".#..#\n.....\n#####\n....#\n...##")
				.searchMonitoringStation()
		assert(moitoring._1.coord.x == 3)
		assert(moitoring._1.coord.y == 4)
		assert(moitoring._2 == 8)
	}

	test("searchMonitoringAsteroid example2") {
		val moitoring = Asteroids.parse("......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####")
				.searchMonitoringStation()
		assert(moitoring._1.coord.x == 5)
		assert(moitoring._1.coord.y == 8)
		assert(moitoring._2 == 33)
	}

	test("searchMonitoringAsteroid example3") {
		val moitoring = Asteroids.parse("#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.")
				.searchMonitoringStation()
		assert(moitoring._1.coord.x == 1)
		assert(moitoring._1.coord.y == 2)
		assert(moitoring._2 == 35)
	}

	test("searchMonitoringAsteroid example4") {
		val moitoring = Asteroids.parse(".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..")
				.searchMonitoringStation()
		assert(moitoring._1.coord.x == 6)
		assert(moitoring._1.coord.y == 3)
		assert(moitoring._2 == 41)
	}

	test("searchMonitoringAsteroid example5") {
		val asteroids = Asteroids.parse(".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##")
		asteroids.asteroids.find(a => a.coord.equals(asCoord(11, 13))).foreach(a => a.getVisibleAsteroids(asteroids).printMap())
		val moitoring = asteroids.searchMonitoringStation()
		assert(moitoring._1.coord.x == 11)
		assert(moitoring._1.coord.y == 13)
		assert(moitoring._2 == 210)
	}

	test("diagonal test from reddit") {
		val asteroids = Asteroids.parse("#.........\n..........\n....#.....\n......#...")
		val visibleAsteroids = asteroids.asteroids.head.getVisibleAsteroids(asteroids)
		assert(visibleAsteroids.get(0, 0) == Visibility.STATION)
		assert(visibleAsteroids.get(4, 2) == Visibility.VISIBLE)
		assert(visibleAsteroids.get(6, 3) == Visibility.INVISIBLE)
	}
}
