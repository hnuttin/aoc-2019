package com.hnuttin.aoc2019.day7

import com.hnuttin.aoc2019.day5.intcode.IntcodeProgram

object AmplifierComputer {

	val numberOfAmplifiers = 5

	def calculateMaxBurst(intcode: List[Int]): Int = {
		List.range(5, 10)
				.permutations
				.map(phases => calculateAmplifierSignal(intcode, phases))
				.max
	}

	private def calculateAmplifierSignal(intcode: List[Int], phases: List[Int]): Int = {
		val amplifiers = phases.map(phase => new IntcodeProgram(intcode))
		calculateAmplifierSignalAccum(amplifiers, phases, 0, 0)
	}

	@scala.annotation.tailrec
	private def calculateAmplifierSignalAccum(amplifiers: List[IntcodeProgram], phases: List[Int], amplifierIndex: Int, input: Int): Int = {
		val inputs = if (phases.isEmpty) List(input) else List(phases.head, input)
		val execution = amplifiers(amplifierIndex).executeUntilOutputOrHalted(inputs)
		if (execution._2.isEmpty && amplifierIndex + 1 == numberOfAmplifiers) {
			execution._1.getOrElse(input)
		} else {
			val newAmplifiers = if (execution._2.isDefined) amplifiers.updated(amplifierIndex, execution._2.get) else amplifiers
			val newAmplifierIndex = if (amplifierIndex + 1 == numberOfAmplifiers) 0 else amplifierIndex + 1
			val newPhases = if (phases.isEmpty) phases else phases.tail
			calculateAmplifierSignalAccum(newAmplifiers, newPhases, newAmplifierIndex, execution._1.getOrElse(input))
		}
	}

}
