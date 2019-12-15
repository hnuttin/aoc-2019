package com.hnuttin.aoc2019.day8

object Day8 extends App {

	println("Password from image data: " + Image.parse(Input.data, new Resolution(25, 6)).calculateChecksum())

}
