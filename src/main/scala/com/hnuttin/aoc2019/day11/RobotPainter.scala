package com.hnuttin.aoc2019.day11

import com.hnuttin.aoc2019.common.Coordinate.asCoord
import com.hnuttin.aoc2019.common.{Coordinate, Direction}
import com.hnuttin.aoc2019.day5.intcode.IntcodeProgram

import scala.collection.immutable.HashMap

class RobotPainter(val program: IntcodeProgram) {

	val hull = new Hull()

	val robot = new Robot()

	def paintPanels(): Unit = {
		hull.paint(asCoord(0, 0), 1)
		paintUntillProgramHalted()
	}

	@scala.annotation.tailrec
	private def paintUntillProgramHalted(): Unit = {
		if (!program.halted) {
			program.executeUntilOutputOrHalted(List(hull.getColor(robot.position)))
			program.executeUntilOutputOrHalted()
			hull.paint(robot.position, colorOutput.intValue)
			movementOutput match {
				case 0L => robot.turnLeft()
				case 1L => robot.turnRight()
			}
			paintUntillProgramHalted()
		}
	}

	private def movementOutput: Long = {
		program.outputs.last
	}

	private def colorOutput: Long = {
		program.outputs(program.outputs.length - 2)
	}
}

class Robot {

	private var _position = asCoord(0, 0)
	private var _direction = Direction.U

	def position: Coordinate = _position

	def turnLeft(): Unit = {
		_direction = Direction.turnLeft(_direction)
		move()
	}

	private def move(): Unit = {
		_position = _position.move(_direction)
	}

	def turnRight(): Unit = {
		_direction = Direction.turnRight(_direction)
		move()
	}
}

class Hull {

	private var panels = HashMap[Int, Map[Int, Panel]]()

	def countPaintedPanels(): Int = panels.values.flatMap(line => line.values).count(p => p.isPainted)

	def getColor(coordinate: Coordinate): Int = getOrCreatePanel(coordinate).color

	def paint(coordinate: Coordinate, color: Int): Unit = getOrCreatePanel(coordinate).paint(color)

	private def getOrCreatePanel(coordinate: Coordinate): Panel = {
		if (panels.contains(coordinate.y)) {
			val line: Map[Int, Panel] = panels(coordinate.y)
			if (!line.contains(coordinate.x)) {
				panels += coordinate.y -> (line + (coordinate.x -> new Panel(0)))
			}
		} else {
			panels += coordinate.y -> HashMap(coordinate.x -> new Panel(0))
		}
		panels(coordinate.y)(coordinate.x)
	}

	def drawHull(): Unit = {
		List.range(minY, maxY + 1).reverse.foreach(y => drawLine(y))
	}

	private def drawLine(y: Int): Unit = {
		List.range(minX, maxX + 1).foreach(x => getOrCreatePanel(asCoord(x, y)).draw())
		println()
	}

	private def minX: Int = {
		panels.flatMap(line => line._2).keys.min
	}

	private def maxX: Int = {
		panels.flatMap(line => line._2).keys.max
	}

	private def minY: Int = {
		panels.keys.min
	}

	private def maxY: Int = {
		panels.keys.max
	}
}

class Panel(var color: Int) {

	var _painted = false

	def isPainted: Boolean = _painted

	def paint(color: Int): Unit = {
		this.color = color
		_painted = true
	}

	def draw(): Unit = {
		color match {
			case 0 => print(' ')
			case 1 => print('#')
		}
	}

}
