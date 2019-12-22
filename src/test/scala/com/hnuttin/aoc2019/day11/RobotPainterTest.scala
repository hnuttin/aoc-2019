package com.hnuttin.aoc2019.day11

import com.hnuttin.aoc2019.day5.intcode.IntcodeProgram
import org.scalatest.FunSuite

class RobotPainterTest extends FunSuite {

	test("day11") {
		val robotPainter = new RobotPainter(IntcodeProgram.fromLongCode(Input.intcode))
		robotPainter.paintPanels()
		println(robotPainter.hull.countPaintedPanels())
	}

}
