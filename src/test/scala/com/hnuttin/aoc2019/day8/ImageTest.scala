package com.hnuttin.aoc2019.day8

import org.scalatest.FunSuite

class ImageTest extends FunSuite {

	test("example1 parse") {
		val image = Image.parse("123456789012", new Resolution(3, 2))
		assert(image.layers.head.lines.head == List(1, 2, 3))
		assert(image.layers.head.lines(1) == List(4, 5, 6))
		assert(image.layers(1).lines.head == List(7, 8, 9))
		assert(image.layers(1).lines(1) == List(0, 1, 2))
	}

	test("example1 checksum") {
		val image = Image.parse("123456789012", new Resolution(3, 2))
		assert(image.calculateChecksum() == 1)
	}

	test("example2 checksum") {
		val image = Image.parse("123452789012", new Resolution(3, 2))
		assert(image.calculateChecksum() == 2)
	}

}
