package com.hnuttin.aoc2019.day4

object PasswordCounter {

	def countValidBetween(min: Int, max: Int): Int = {
		countValidBetweenInner(new Password(min), 0, min, max)
	}

	@scala.annotation.tailrec
	private def countValidBetweenInner(currentPassword: Password, count: Int, min: Int, max: Int): Int = {
		if (currentPassword.value > max) {
			count
		} else if (currentPassword.isValid(min, max)) {
			countValidBetweenInner(currentPassword.increment, count + 1, min, max)
		} else {
			countValidBetweenInner(currentPassword.increment, count, min, max)
		}
	}

}
