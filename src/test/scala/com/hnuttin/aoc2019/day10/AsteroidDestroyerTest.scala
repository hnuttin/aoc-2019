package com.hnuttin.aoc2019.day10

import com.hnuttin.aoc2019.common.Coordinate.asCoord
import org.scalatest.FunSuite

class AsteroidDestroyerTest extends FunSuite {

	test("example") {
		val asteroids = Asteroids.parse(".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....X...###..\n..#.#.....#....##")
		val destroyed = new AsteroidDestroyer(asteroids).destroy(new Asteroid(asCoord(8, 3)))
		assert(destroyed == List(
			asCoord(8, 1),
			asCoord(9, 0),
			asCoord(9, 1),
			asCoord(10, 0),
			asCoord(9, 2),
			asCoord(11, 1),
			asCoord(12, 1),
			asCoord(11, 2),
			asCoord(15, 1),
			asCoord(12, 2),
			asCoord(13, 2),
			asCoord(14, 2),
			asCoord(15, 2),
			asCoord(12, 3),
			asCoord(16, 4),
			asCoord(15, 4),
			asCoord(10, 4),
			asCoord(4, 4),
			asCoord(2, 4),
			asCoord(2, 3),
			asCoord(0, 2),
			asCoord(1, 2),
			asCoord(0, 1),
			asCoord(1, 1),
			asCoord(5, 2),
			asCoord(1, 0),
			asCoord(5, 1),
			asCoord(6, 1),
			asCoord(6, 0),
			asCoord(7, 0),
			asCoord(8, 0),
			asCoord(10, 1),
			asCoord(14, 0),
			asCoord(16, 1),
			asCoord(13, 3),
			asCoord(14, 3),
		))
	}

	test("large example") {
		val asteroids = Asteroids.parse(".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##")
		val station = asteroids.searchMonitoringStation()
		val destroyed = new AsteroidDestroyer(asteroids).destroy(station._1)
		assert(destroyed(199) == asCoord(8, 2))
	}

}
