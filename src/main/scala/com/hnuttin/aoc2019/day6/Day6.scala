package com.hnuttin.aoc2019.day6

object Day6 extends App {

	println("Orbits from YOU to SAN: " + OrbitMap.parse(Input.input).calculateOrbits("YOU", "SAN"))

}
