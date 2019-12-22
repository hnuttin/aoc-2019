package com.hnuttin.aoc2019.day10

import com.hnuttin.aoc2019.common.Coordinate
import com.hnuttin.aoc2019.common.Coordinate.asCoord

class Asteroid(val coord: Coordinate) {

	def getVisibleAsteroids(asteroids: Asteroids): VisibleAsteroids = {
		val visibleAsteroids = VisibleAsteroids.initialize(asteroids.width, asteroids.height)
		visibleAsteroids.markStation(coord)
		List.range(0, asteroids.height)
				.flatMap(y => List.range(0, asteroids.width).map(x => asCoord(x, y)))
				.filter(coordinate => !coordinate.equals(coord))
				.foreach(coordinate => mark(visibleAsteroids, asteroids, coordinate))
		visibleAsteroids
	}

	override def toString: String = coord.toString

	private def mark(visibleAsteroids: VisibleAsteroids, asteroids: Asteroids, currentCoord: Coordinate): Unit = {
		if (asteroids.existsAt(currentCoord)) {
			if (visibleAsteroids.isUnknown(currentCoord)) {
				visibleAsteroids.markVisible(currentCoord)
			}
			val rawTravel = currentCoord.minus(coord)
			val divisor = Math.abs(gcd(rawTravel.x, rawTravel.y))
			val minimumTravel = rawTravel.divide(divisor)
			markInvisibleForTravel(visibleAsteroids, currentCoord.add(minimumTravel), minimumTravel)
		}
	}

	@scala.annotation.tailrec
	private def gcd(a: Int, b: Int): Int = {
		if (b == 0) a else gcd(b, a % b)
	}

	@scala.annotation.tailrec
	private def markInvisibleForTravel(visibleAsteroids: VisibleAsteroids, invisibleCoord: Coordinate, travel: Coordinate): Unit = {
		if (visibleAsteroids.isWithinBounds(invisibleCoord)) {
			visibleAsteroids.markInvisible(invisibleCoord)
			markInvisibleForTravel(visibleAsteroids, invisibleCoord.add(travel), travel)
		}
	}
}

class Asteroids(val asteroids: List[Asteroid], val width: Int, val height: Int) {

	def existsAt(coordinate: Coordinate): Boolean = asteroids.exists(a => a.coord.equals(coordinate))

	def searchMonitoringStation(): (Asteroid, Int) = {
		asteroids
				.map(a => Tuple2(a, a.getVisibleAsteroids(this).countVisible()))
				.maxBy(tuple => tuple._2)
	}

	def remove(coordinates: List[Coordinate]): Asteroids = {
		new Asteroids(asteroids.filter(a => !coordinates.contains(a.coord)), width, height)
	}

}

object Asteroids {

	def parse(input: String): Asteroids = {
		val lines = input.split("\n")
		val asteroids = lines
				.zipWithIndex
				.flatMap(lineToAsteroids)
				.toList
		new Asteroids(asteroids, lines.head.length, lines.length)
	}

	private def lineToAsteroids(lineWithIndex: (String, Int)): IndexedSeq[Asteroid] = {
		lineWithIndex._1
				.zipWithIndex
				.filter(charWithIndex => charWithIndex._1 == '#')
				.map(charWithIndex => new Asteroid(asCoord(charWithIndex._2, lineWithIndex._2)))
	}

}
