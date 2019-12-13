package com.hnuttin.aoc2019.day1

object FuelCounterUpper {

	def calculateTotalFuel(modules: List[Int]): Int = {
		modules.map(calculateModuleFuel).sum
	}

	private def calculateModuleFuel(mass: Int): Int = {
		calculateAccumulatedFuel(calculateFuelForMass(mass), calculateFuelForMass(mass))
	}

	@scala.annotation.tailrec
	private def calculateAccumulatedFuel(accumFuel: Int, extraFuel: Int): Int = {
		val fuel = calculateFuelForMass(extraFuel)
		if (fuel > 0) {
			calculateAccumulatedFuel(accumFuel + fuel, fuel)
		} else {
			accumFuel
		}
	}

	private def calculateFuelForMass(mass: Int): Int = {
		minusTwo(roundDown(divideByThree(mass)))
	}

	private def divideByThree(module: Int): Double = {
		module / 3
	}

	private def roundDown(value: Double): Int = {
		math.floor(value).toInt
	}

	private def minusTwo(value: Int): Int = {
		value - 2
	}

}
