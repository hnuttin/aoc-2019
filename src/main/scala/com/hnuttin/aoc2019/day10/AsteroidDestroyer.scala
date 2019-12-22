package com.hnuttin.aoc2019.day10

import com.hnuttin.aoc2019.common.Coordinate

class AsteroidDestroyer(val asteroids: Asteroids) {

	def destroy(station: Asteroid): List[Coordinate] = {
		shootFromStation(station, asteroids, List())
	}

	@scala.annotation.tailrec
	private def shootFromStation(station: Asteroid, asteroidsLeft: Asteroids, destroyedAccum: List[Coordinate]): List[Coordinate] = {
		val visibleAsteroids = station.getVisibleAsteroids(asteroidsLeft)
		if (visibleAsteroids.anyVisible) {
			val destroyedQ1 = visibleAsteroids.getVisibleInQ1(station.coord).sortBy(c => angleQ1(c, station))
			val destroyedQ2 = visibleAsteroids.getVisibleInQ2(station.coord).sortBy(c => angleQ2(c, station))
			val destroyedQ3 = visibleAsteroids.getVisibleInQ3(station.coord).sortBy(c => angleQ3(c, station))
			val destroyedQ4 = visibleAsteroids.getVisibleInQ4(station.coord).sortBy(c => angleQ4(c, station))
			val destroyed = destroyedAccum.appendedAll(destroyedQ1).appendedAll(destroyedQ2).appendedAll(destroyedQ3).appendedAll(destroyedQ4)
			shootFromStation(station, asteroidsLeft.remove(visibleAsteroids.visibleCoordinates), destroyed)
		} else {
			destroyedAccum
		}
	}

	private def angleQ1(coordinate: Coordinate, station: Asteroid): Double = {
		val travel = Coordinate.asCoord(coordinate.x - station.coord.x, station.coord.y - coordinate.y)
		if (travel.y == 0) 0D else 1D * travel.x / travel.y
	}

	private def angleQ2(coordinate: Coordinate, station: Asteroid): Double = {
		val travel = Coordinate.asCoord(coordinate.x - station.coord.x, coordinate.y - station.coord.y)
		if (travel.x == 0) 0D else 1D * travel.y / travel.x
	}

	private def angleQ3(coordinate: Coordinate, station: Asteroid): Double = {
		val travel = Coordinate.asCoord(station.coord.x - coordinate.x, coordinate.y - station.coord.y)
		if (travel.y == 0) 0D else 1D * travel.x / travel.y
	}

	private def angleQ4(coordinate: Coordinate, station: Asteroid): Double = {
		val travel = Coordinate.asCoord(station.coord.x - coordinate.x, station.coord.y - coordinate.y)
		if (travel.x == 0) 0D else 1D * travel.y / travel.x
	}

}