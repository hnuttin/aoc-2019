package com.hnuttin.aoc2019.day10

class Asteroid(val x: Int, val y: Int) {
	def detectOtherAsteroids(asteroids: Asteroids, width: Int, height: Int): Int = {
		val visibleMap = Array.fill(height)(Array.fill(width)(-1))
		visibleMap(y)(x) = 0
		List.range(0, height)
				.flatMap(y => List.range(0, width).map(x => Tuple2(x, y)))
				.filter(coordinate => !(coordinate._1 == x && coordinate._2 == y))
				.foreach(coordinate => mark(visibleMap, asteroids, coordinate))
		visibleMap.flatten.count(v => v == 1)
	}

	private def mark(visibleMap: Array[Array[Int]], asteroids: Asteroids, coordinate: (Int, Int)): Unit = {
		if (asteroids.existsAt(coordinate._1, coordinate._2)) {
			if (visibleMap(coordinate._2)(coordinate._1) == -1) {
				visibleMap(coordinate._2)(coordinate._1) = 1
			}
			if (coordinate._1 == x) {
				markInvisibleAboveOrBelow(visibleMap, coordinate)
			} else if (coordinate._2 == y) {
				markInvisibleLeftOrRight(visibleMap, coordinate)
			} else {
				val rawTravel = Tuple2(coordinate._1 - x, coordinate._2 - y)
				val travel = calculateMinimalDiagonalTravel(rawTravel)
				markInvisibleDiagonal(travel, Tuple2(coordinate._1 + travel._1, coordinate._2 + travel._2), visibleMap);
			}
		}
	}

	@scala.annotation.tailrec
	private def calculateMinimalDiagonalTravel(travel: (Int, Int)): (Int, Int) = {
		if (travel._1 % 2 == 0 && travel._2 % 2 == 0) {
			calculateMinimalDiagonalTravel(Tuple2(travel._1 / 2, travel._2 / 2))
		} else {
			travel
		}
	}

	private def markInvisibleAboveOrBelow(visibleMap: Array[Array[Int]], coordinate: (Int, Int)): Unit = {
		if (coordinate._2 > y) {
			List.range(coordinate._2 + 1, visibleMap.length).foreach(y => visibleMap(y)(x) = 0)
		} else {
			List.range(0, coordinate._2).foreach(y => visibleMap(y)(x) = 0)
		}
	}

	private def markInvisibleLeftOrRight(visibleMap: Array[Array[Int]], coordinate: (Int, Int)): Unit = {
		if (coordinate._1 > x) {
			List.range(coordinate._1 + 1, visibleMap.head.length).foreach(x => visibleMap(y)(x) = 0)
		} else {
			List.range(0, coordinate._1).foreach(x => visibleMap(y)(x) = 0)
		}
	}

	@scala.annotation.tailrec
	private def markInvisibleDiagonal(travel: (Int, Int), coordinate: (Int, Int), visibleMap: Array[Array[Int]]): Unit = {
		if (coordinate._1 >= 0 && coordinate._1 < visibleMap.head.length && coordinate._2 >= 0 && coordinate._2 < visibleMap.length) {
			visibleMap(coordinate._2)(coordinate._1) = 0
			markInvisibleDiagonal(travel, Tuple2(coordinate._1 + travel._1, coordinate._2 + travel._2), visibleMap)
		}
	}

}

class Asteroids(val asteroids: List[Asteroid]) {
	def existsAt(x: Int, y: Int): Boolean = asteroids.exists(a => a.x == x && a.y == y)

	def searchMonitoringAsteroid(width: Int, height: Int): (Asteroid, Int) = {
		asteroids
				.map(a => Tuple2(a, a.detectOtherAsteroids(this, width, height)))
				.maxBy(tuple => tuple._2)
	}
}

object Asteroids {

	def parse(input: String): Asteroids = {
		new Asteroids(input.split("\n")
				.zipWithIndex
				.flatMap(lineToAsteroids)
				.toList)
	}

	private def lineToAsteroids(lineWithIndex: (String, Int)): IndexedSeq[Asteroid] = {
		lineWithIndex._1
				.zipWithIndex
				.filter(charWithIndex => charWithIndex._1 == '#')
				.map(charWithIndex => new Asteroid(charWithIndex._2, lineWithIndex._2))
	}

}
