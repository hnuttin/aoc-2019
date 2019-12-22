package com.hnuttin.aoc2019.day10

import com.hnuttin.aoc2019.common.Coordinate

object Day10 extends App {

	val asteroids = Asteroids.parse(Input.input)
	val station: (Asteroid, Int) = asteroids.searchMonitoringStation()

	println("Asteroids on monitoring stattion: " + station._2)

	val destroyed = new AsteroidDestroyer(asteroids).destroy(station._1)
	val twoHDestroyed: Coordinate = destroyed(199)

	println("200th destroyed ateroid: " + (twoHDestroyed.x * 100 + twoHDestroyed.y))

}
