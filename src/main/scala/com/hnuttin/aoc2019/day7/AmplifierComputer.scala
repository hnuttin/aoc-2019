package com.hnuttin.aoc2019.day7

import com.hnuttin.aoc2019.day5.intcode.IntcodeProgram

object AmplifierComputer {

	val numberOfAmplifiers = 5

	def calculateMaxBurst(intcode: List[Int]): Long = {
		List.range(5L, 10L)
				.permutations
				.map(phases => calculateAmplifierSignal(intcode, phases))
				.max
	}

	private def calculateAmplifierSignal(intcode: List[Int], phases: List[Long]): Long = {
		val amplifiers = phases.map(phase => IntcodeProgram.fromIntcode(intcode))
		calculateAmplifierSignalAccum(amplifiers, phases, 0, 0)
	}

	@scala.annotation.tailrec
	private def calculateAmplifierSignalAccum(amplifiers: List[IntcodeProgram], phases: List[Long], amplifierIndex: Int, input: Long): Long = {
		val inputs = if (phases.isEmpty) List(input) else List(phases.head, input)
		val amplifier = amplifiers(amplifierIndex)
		amplifier.executeUntilOutputOrHalted(inputs)
		if (amplifier.halted && amplifierIndex + 1 == numberOfAmplifiers) {
			amplifier.outputs.last
		} else {
			val newAmplifierIndex = if (amplifierIndex + 1 == numberOfAmplifiers) 0 else amplifierIndex + 1
			val newPhases = if (phases.isEmpty) phases else phases.tail
			calculateAmplifierSignalAccum(amplifiers, newPhases, newAmplifierIndex, amplifier.outputs.last)
		}
	}

}
