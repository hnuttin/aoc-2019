package com.hnuttin.aoc2019.day1

object FuelCounterUpper {

	def calculate(modules: List[Int]): Int = {
		modules.map(divideByThree).map(roundDown).map(minusTwo).sum
	}

	private def divideByThree(module: Int): Double = {
		module / 3;
	}

	private def roundDown(value: Double): Int = {
		math.floor(value).toInt
	}

	private def minusTwo(value: Int): Int = {
		value - 2
	}

}
