package com.hnuttin.aoc2019.day5

import java.io.{ByteArrayInputStream, StringReader}

import org.scalatest.FunSuite

class IntcodeProgramTest extends FunSuite {

	test("3,0,4,0,99") {
		Console.withIn(new StringReader("50"))(new IntcodeProgram(List(3,0,4,0,99), 0).execute())
	}

	test("1002,4,3,4,33") {
		new IntcodeProgram(List(1002,4,3,4,33), 0).execute()
	}

}
