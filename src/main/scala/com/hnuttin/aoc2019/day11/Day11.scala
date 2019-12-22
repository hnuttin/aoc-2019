package com.hnuttin.aoc2019.day11

import com.hnuttin.aoc2019.day5.intcode.IntcodeProgram

object Day11 extends App {

	private val robotPainter = new RobotPainter(IntcodeProgram.fromLongCode(Input.intcode))
	robotPainter.paintPanels()
	println("Number of panels painted: " + robotPainter.hull.countPaintedPanels())
	robotPainter.hull.drawHull()

}
