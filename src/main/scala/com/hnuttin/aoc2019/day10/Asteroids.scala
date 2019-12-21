package com.hnuttin.aoc2019.day10

import com.hnuttin.aoc2019.common.Coordinate
import com.hnuttin.aoc2019.common.Coordinate.asCoord

class Asteroid(val asteroidCoord: Coordinate) {

	def constructVisibilityMap(asteroids: Asteroids, width: Int, height: Int): VisibilityMap = {
		val visibleMap = VisibilityMap.initialize(width, height)
		visibleMap.markStation(asteroidCoord)
		List.range(0, height)
				.flatMap(y => List.range(0, width).map(x => asCoord(x, y)))
				.filter(coordinate => !coordinate.equals(asteroidCoord))
				.foreach(coordinate => mark(visibleMap, asteroids, coordinate))
		visibleMap
	}

	private def mark(visibleMap: VisibilityMap, asteroids: Asteroids, currentCoord: Coordinate): Unit = {
		if (asteroids.existsAt(currentCoord)) {
			if (visibleMap.isUnknown(currentCoord)) {
				visibleMap.markVisible(currentCoord)
			}
			if (currentCoord.isSameWidth(asteroidCoord)) {
				if (currentCoord.isBelow(asteroidCoord)) {
					visibleMap.markInvisibleBelow(currentCoord)
				} else {
					visibleMap.markInvisibleAbove(currentCoord)
				}
			} else if (currentCoord.isSameHeight(asteroidCoord)) {
				if (currentCoord.isRightOff(asteroidCoord)) {
					visibleMap.markInvisibleRightOff(currentCoord)
				} else {
					visibleMap.markInvisibleLeftOff(currentCoord)
				}
			} else {
				val rawTravel = currentCoord.minus(asteroidCoord)
				val gcd = Math.abs(gcdResursive(rawTravel.x, rawTravel.y))
				val minimumTravel = rawTravel.divide(gcd)
				markInvisibleDiagonal(visibleMap, currentCoord.add(minimumTravel), minimumTravel)
			}
		}
	}

	@scala.annotation.tailrec
	private def gcdResursive(a: Int, b: Int): Int = {
		if (b == 0) a else gcdResursive(b, a % b)
	}

	@scala.annotation.tailrec
	private def markInvisibleDiagonal(visibleMap: VisibilityMap, invisibleCoord: Coordinate, travel: Coordinate): Unit = {
		if (visibleMap.isWithinBounds(invisibleCoord)) {
			visibleMap.markInvisible(invisibleCoord)
			markInvisibleDiagonal(visibleMap, invisibleCoord.add(travel), travel)
		}
	}

}

class Asteroids(val asteroids: List[Asteroid]) {

	def existsAt(coordinate: Coordinate): Boolean = asteroids.exists(a => a.asteroidCoord.equals(coordinate))

	def searchMonitoringAsteroid(width: Int, height: Int): (Asteroid, Int) = {
		asteroids
				.map(a => Tuple2(a, a.constructVisibilityMap(this, width, height).countVisible()))
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
				.map(charWithIndex => new Asteroid(asCoord(charWithIndex._2, lineWithIndex._2)))
	}

}
