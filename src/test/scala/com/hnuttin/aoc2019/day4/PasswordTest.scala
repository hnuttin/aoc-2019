package com.hnuttin.aoc2019.day4

import org.scalatest.FunSuite

class PasswordTest extends FunSuite {

	test("111111 is valid") {
		assert(isValid(111111))
	}

	test("122345 is valid") {
		assert(isValid(122345))
	}

	test("111123 is valid") {
		assert(isValid(111123))
	}

	test("135579 is valid") {
		assert(isValid(135579))
	}

	test("223450 is invalid") {
		assert(isInvalid(223450))
	}

	test("123789 is invalid") {
		assert(isInvalid(123789))
	}

	private def isValid(value: Int) = {
		new Password(value).isValid(0, 999999)
	}

	private def isInvalid(value: Int) = {
		!isValid(value)
	}

}
