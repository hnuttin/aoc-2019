package com.hnuttin.aoc2019.day4

class Password(val value: Int) {

	def increment: Password = {
		new Password(value + 1)
	}

	def isValid(min: Int, max: Int): Boolean = isSixDigits && isWithinRange(min, max) && hasExactlyTwoAdjacentDigits && digitsNeverDecrease

	private def valueAsString: String = String.valueOf(value)

	private def digitLength = valueAsString.length

	private def isSixDigits: Boolean = digitLength == 6

	private def isWithinRange(min: Int, max: Int): Boolean = value > min && value < max

	private def hasExactlyTwoAdjacentDigits: Boolean = hasExactlyTwoAdjacentDigitsFromPosition(0)

	@scala.annotation.tailrec
	private def hasExactlyTwoAdjacentDigitsFromPosition(position: Int): Boolean = {
		if (noDigitAtPosition(position + 1)) {
			false
		} else {
			hasExactlyTwoAdjacentValuesAtPosition(position) || hasExactlyTwoAdjacentDigitsFromPosition(position + 1)
		}
	}

	private def hasExactlyTwoAdjacentValuesAtPosition(position: Int) = {
		sameDigitAsNext(position) &&
				(noDigitAtPosition(position + 2) || !sameDigitAsNext(position + 1)) &&
				(noDigitAtPosition(position - 1) || !sameDigitAsNext(position - 1))
	}

	private def noDigitAtPosition(position: Int) = {
		position >= digitLength || position < 0
	}

	private def sameDigitAsNext(position: Int) = {
		digitAtPosition(position) == digitAtPosition(position + 1)
	}

	private def digitAtPosition(position: Int): Int = Integer.parseInt(valueAsString.substring(position, position + 1))

	private def digitsNeverDecrease: Boolean = digitsNeverDecreaseFromPosition(digitAtPosition(0), 1)

	@scala.annotation.tailrec
	private def digitsNeverDecreaseFromPosition(previousValue: Int, position: Int): Boolean = {
		if (position >= digitLength) {
			true
		} else {
			val currentValue = digitAtPosition(position)
			previousValue <= currentValue && digitsNeverDecreaseFromPosition(currentValue, position + 1)
		}
	}
}
