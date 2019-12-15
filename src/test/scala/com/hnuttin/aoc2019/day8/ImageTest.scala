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

	test("decode1") {
		Image.parse("0222112222120000", new Resolution(2, 2)).decode()
	}

	test("decode2") {
		Image.parse("122202221122221200020001", new Resolution(2, 2)).decode()
	}

	test("input resolution") {
		val resolution = Image.parse(Input.data, new Resolution(25, 6)).resolution
		assert(resolution.width == 25)
		assert(resolution.height == 6)
	}

	test("input parse") {
		val image = Image.parse(Input.data, new Resolution(25, 6))
		assert(image.layers.head.lines.head == toLine("2222222222222221222222222"))
		assert(image.layers.head.lines(1) == toLine("2222212212202202222222212"))
		assert(image.layers.last.lines.last == toLine("0221201012100101010100120"))
	}

	test("example1 pixelAtPosition") {
		val image = Image.parse("123456789012", new Resolution(3, 2))
		assert(image.layers.head.pixelAtPosition(0, 0) == 1)
		assert(image.layers.head.pixelAtPosition(1, 0) == 2)
		assert(image.layers.head.pixelAtPosition(2, 0) == 3)
		assert(image.layers.head.pixelAtPosition(0, 1) == 4)
		assert(image.layers.head.pixelAtPosition(1, 1) == 5)
		assert(image.layers.head.pixelAtPosition(2, 1) == 6)
		assert(image.layers.last.pixelAtPosition(0, 0) == 7)
		assert(image.layers.last.pixelAtPosition(1, 0) == 8)
		assert(image.layers.last.pixelAtPosition(2, 0) == 9)
		assert(image.layers.last.pixelAtPosition(0, 1) == 0)
		assert(image.layers.last.pixelAtPosition(1, 1) == 1)
		assert(image.layers.last.pixelAtPosition(2, 1) == 2)
	}

	private def toLine(line: String): List[Int] = {
		line.split("").map(pixel => Integer.parseInt(pixel)).toList
	}
}
