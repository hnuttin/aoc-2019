package com.hnuttin.aoc2019.day1

import org.scalatest.FunSuite

class FuelCounterUpperTest extends FunSuite {

	test("example1") {
		assert(FuelCounterUpper.calculateTotalFuel(14 :: Nil) == 2)
	}

	test("example2") {
		assert(FuelCounterUpper.calculateTotalFuel(1969 :: Nil) == 966)
	}

	test("example3") {
		assert(FuelCounterUpper.calculateTotalFuel(100756 :: Nil) == 50346)
	}

}
