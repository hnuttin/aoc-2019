package com.hnuttin.aoc2019.day4

import org.scalatest.FunSuite

class PasswordTest extends FunSuite {

	test("111111 is invalid") {
		assert(isInvalid(111111))
	}

	test("122345 is valid") {
		assert(isValid(122345))
	}

	test("111123 is invalid") {
		assert(isInvalid(111123))
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

	test("123339 is invalid") {
		assert(isInvalid(123339))
	}

	test("123444 is invalid") {
		assert(isInvalid(123444))
	}

	test("112233 is valid") {
		assert(isValid(112233))
	}

	test("111122 is valid") {
		assert(isValid(111122))
	}

	private def isValid(value: Int) = {
		new Password(value).isValid(0, 999999)
	}

	private def isInvalid(value: Int) = {
		!isValid(value)
	}

}
