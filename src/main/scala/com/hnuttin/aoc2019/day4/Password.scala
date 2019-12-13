package com.hnuttin.aoc2019.day4

class Password(val value: Int) {

	def increment: Password = {
		new Password(value + 1)
	}

	def isValid(min: Int, max: Int): Boolean = isSixDigits && isWithinRange(min, max) && hasTwoAdjacentDigits && digitsNeverDecrease

	private def valueAsString: String = String.valueOf(value)

	private def isSixDigits: Boolean = valueAsString.length == 6

	private def isWithinRange(min: Int, max: Int): Boolean = value > min && value < max

	private def hasTwoAdjacentDigits: Boolean = hasTwoAdjacentDigitsFromPosition(0)

	@scala.annotation.tailrec
	private def hasTwoAdjacentDigitsFromPosition(position: Int): Boolean = {
		if (positionAtEndOfValue(position)) false else areSameDigits(position) || hasTwoAdjacentDigitsFromPosition(position + 1)
	}

	private def positionAtEndOfValue(position: Int) = {
		position + 1 >= valueAsString.length
	}

	private def areSameDigits(position: Int) = {
		digitAtPosition(position) == digitAtPosition(position + 1)
	}

	private def digitAtPosition(position: Int): Int = Integer.parseInt(valueAsString.substring(position, position + 1))

	private def digitsNeverDecrease: Boolean = digitsNeverDecreaseFromPosition(digitAtPosition(0), 1)

	@scala.annotation.tailrec
	private def digitsNeverDecreaseFromPosition(previousValue: Int, position: Int): Boolean = {
		if (position >= valueAsString.length) {
			true
		} else {
			val currentValue = digitAtPosition(position)
			previousValue <= currentValue && digitsNeverDecreaseFromPosition(currentValue, position + 1)
		}
	}
}
