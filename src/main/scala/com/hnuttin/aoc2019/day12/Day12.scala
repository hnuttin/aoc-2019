package com.hnuttin.aoc2019.day12

object Day12 extends App {

	private var moons: Moons = Moons.parse(Input.input)
	moons.simulate(1000)
	println("Total energy after 1000 steps: " + moons.totalEnergy)

	moons = Moons.parse(Input.input)
	println("Steps until loop: " + moons.simulateLoop())

}
