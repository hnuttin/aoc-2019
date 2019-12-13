package com.hnuttin.aoc2019.day3

object Day3 extends App {
	println("Manhatten distance closest intersection: " + Grid.parse(new Coordinate(1, 1), Input.wire1, Input.wire2).manhattenDistanceClosestIntersection)
	println("Fewest steps to intersection: " + Grid.parse(new Coordinate(1, 1), Input.wire1, Input.wire2).fewestStepsToInteraction)
}
