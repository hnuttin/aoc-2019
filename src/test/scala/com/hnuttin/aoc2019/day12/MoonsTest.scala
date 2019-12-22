package com.hnuttin.aoc2019.day12

import com.hnuttin.aoc2019.day12.Coordinate3D.asCoord3D
import org.scalatest.FunSuite

class MoonsTest extends FunSuite {

	test("parse") {
		val moons = Moons.parse("<x=-10, y=-10, z=-13>\n<x=5, y=5, z=-9>\n<x=3, y=8, z=-16>\n<x=1, y=3, z=-3>")
		assert(moons.moons.length == 4)
		assert(moons.moons.head.position == asCoord3D(-10, -10, -13))
		assert(moons.moons.head.velocity == asCoord3D(0, 0, 0))
		assert(moons.moons(1).position == asCoord3D(5, 5, -9))
		assert(moons.moons(1).velocity == asCoord3D(0, 0, 0))
	}

	test("simulate example1") {
		val moons = Moons.parse("<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>")
		moons.simulate(10)
		assert(moons.moons.head.position == asCoord3D(2, 1, -3))
		assert(moons.moons.head.velocity == asCoord3D(-3, -2, 1))
		assert(moons.moons(1).position == asCoord3D(1, -8, 0))
		assert(moons.moons(1).velocity == asCoord3D(-1, 1, 3))
		assert(moons.moons(2).position == asCoord3D(3, -6, 1))
		assert(moons.moons(2).velocity == asCoord3D(3, 2, -3))
		assert(moons.moons(3).position == asCoord3D(2, 0, 4))
		assert(moons.moons(3).velocity == asCoord3D(1, -1, -1))
	}

	test("total energy exmaple1") {
		val moons = Moons.parse("<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>")
		moons.simulate(10)
		assert(moons.totalEnergy == 179)
	}

	test("simulate example2") {
		val moons = Moons.parse("<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>")
		moons.simulate(100)
		assert(moons.moons.head.position == asCoord3D(8, -12, -9))
		assert(moons.moons.head.velocity == asCoord3D(-7, 3, 0))
		assert(moons.moons(1).position == asCoord3D(13, 16, -3))
		assert(moons.moons(1).velocity == asCoord3D(3, -11, -5))
		assert(moons.moons(2).position == asCoord3D(-29, -11, -1))
		assert(moons.moons(2).velocity == asCoord3D(-3, 7, 4))
		assert(moons.moons(3).position == asCoord3D(16, -13, 23))
		assert(moons.moons(3).velocity == asCoord3D(7, 1, 1))
	}

	test("total anergy example2") {
		val moons = Moons.parse("<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>")
		moons.simulate(100)
		assert(moons.totalEnergy == 1940)
	}

	test("loop day12") {
		val moons = Moons.parse(Input.input)
		moons.simulateLoop()
	}

}
