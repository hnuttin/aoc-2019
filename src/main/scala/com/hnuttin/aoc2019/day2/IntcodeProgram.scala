package com.hnuttin.aoc2019.day2

object IntcodeProgram {

	def calculate(input: List[Int]): Int = {
		val candidates: IndexedSeq[Tuple2[Int, Int]] = createCandidates()
		val solution = candidates.find(nounVerb => isSolution(input, nounVerb._1, nounVerb._2)).get
		100 * solution._1 + solution._2
	}

	private def createCandidates(): IndexedSeq[(Int, Int)] = {
		val range = 0 to 99
		range.reverse.flatMap(noun => range.reverse.map(verb => Tuple2(noun, verb)))
	}

	private def isSolution(input: List[Int], noun: Int, verb: Int): Boolean = {
		val intcode = new Intcode(input.updated(1, noun).updated(2, verb))
		intcode.execute().codes.head == 19690720
	}

}
