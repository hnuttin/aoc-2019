package com.hnuttin.aoc2019.day8

class Layer(val lines: List[List[Int]]) {

	def numberOfDigits(digit: Int): Int = {
		lines.flatten.count(pixel => pixel == digit)
	}

	def pixelAtPosition(x: Int, y: Int): Int = lines(y)(x)

}

class NumberOfDigitsOrdering(val digit: Int) extends Ordering[Layer] {
	def compare(a: Layer, b: Layer): Int = a.numberOfDigits(digit) compare b.numberOfDigits(digit)
}

class Image(val layers: List[Layer]) {

	def decode(): Unit = {
		for (y <- 0 until resolution.height) {
			for (x <- 0 until resolution.width) {
				decodePixel(x, y)
			}
			println()
		}
	}

	def calculateChecksum(): Int = {
		val layerWithFewestZeros = layers.min(new NumberOfDigitsOrdering(0))
		layerWithFewestZeros.numberOfDigits(1) * layerWithFewestZeros.numberOfDigits(2)
	}

	def resolution: Resolution = new Resolution(layers.head.lines.head.length, layers.head.lines.length)

	private def decodePixel(x: Int, y: Int): Unit = {
		val pixelChar = layers
				.map(layer => layer.pixelAtPosition(x, y))
				.find(pixel => pixel < 2)
				.map(pixel => if (pixel == 1) '█' else ' ')
				.get
		print(pixelChar)
		print(' ')
	}

}

class Resolution(val width: Int, val height: Int) {

}

object Image {
	def parse(data: String, resolution: Resolution): Image = {
		parseAccum(List(), List(), List(), data, resolution)
	}

	@scala.annotation.tailrec
	private def parseAccum(lineAccum: List[Int], layerAccum: List[List[Int]], imageAccuum: List[Layer], dataLeft: String, resolution: Resolution): Image = {
		if (dataLeft.isEmpty) {
			new Image(imageAccuum)
		} else {
			if (lineAccum.length + 1 == resolution.width) {
				if (layerAccum.length + 1 == resolution.height) {
					parseAccum(List(), List(), acummImage(lineAccum, layerAccum, imageAccuum, dataLeft), dataLeft.tail, resolution)
				} else {
					parseAccum(List(), acummLayer(lineAccum, layerAccum, dataLeft), imageAccuum, dataLeft.tail, resolution)
				}
			} else {
				parseAccum(accumLine(lineAccum, dataLeft), layerAccum, imageAccuum, dataLeft.tail, resolution)
			}
		}
	}

	private def acummImage(lineAccum: List[Int], layerAccum: List[List[Int]], imageAccuum: List[Layer], dataLeft: String): List[Layer] = {
		imageAccuum.appended(new Layer(acummLayer(lineAccum, layerAccum, dataLeft)))
	}

	private def acummLayer(lineAccum: List[Int], layerAccum: List[List[Int]], dataLeft: String): List[List[Int]] = {
		layerAccum.appended(accumLine(lineAccum, dataLeft))
	}

	private def accumLine(lineAccum: List[Int], dataLeft: String): List[Int] = {
		lineAccum.appended(firstCharAsInteger(dataLeft))
	}

	private def firstCharAsInteger(dataLeft: String): Int = {
		Integer.parseInt(dataLeft.split("").head)
	}
}
