package com.hnuttin.aoc2019.day4

import org.scalatest.FunSuite

class PasswordCounterTest extends FunSuite {

	test("count example1") {
		assert(PasswordCounter.countValidBetween(123456, 123476) == 1)
	}

	test("count example2") {
		assert(PasswordCounter.countValidBetween(123456, 123556) == 5)
	}

}
