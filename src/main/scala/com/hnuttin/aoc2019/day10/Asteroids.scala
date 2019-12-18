package com.hnuttin.aoc2019.day10

class Asteroid(val x: Int, val y: Int) {
	def detectOtherAsteroids(asteroids: Asteroids, width: Int, height: Int): Int = {
		val visibleMap = Array.fill(height)(Array.fill(width)(-1))
		List.range(0, width)
				.flatMap(x => List.range(0, height).map(y => Tuple2(x, y)))
				.foreach(coordinate => mark(visibleMap, asteroids, coordinate))
		visibleMap.flatten.count(v => v == 1)
	}

	private def mark(visibleMap: Array[Array[Int]], asteroids: Asteroids, coordinate: Tuple2[Int, Int]): Unit = {
		if (asteroids.existsAt(coordinate._1, coordinate._2)) {
			if (visibleMap(coordinate._1)(coordinate._2) == -1) {
				visibleMap(coordinate._1)(coordinate._2) = 1
			}
			// mark asteroids out of los as invisible
		}
	}

	override def equals(other: Any): Boolean = other match {
		case that: Asteroid =>
			(that canEqual this) &&
					x == that.x &&
					y == that.y
		case _ => false
	}

	def canEqual(other: Any): Boolean = other.isInstanceOf[Asteroid]

	override def hashCode(): Int = {
		val state = Seq(x, y)
		state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
	}
}

class Asteroids(val asteroids: List[Asteroid]) {
	def existsAt(x: Int, y: Int): Boolean = asteroids.exists(a => a.x == x && a.y == y)
}

object Asteroids {

	def parse(input: String): Asteroids = {
		new Asteroids(input.split("\n")
				.zipWithIndex
				.flatMap(lineToAsteroids)
				.toList)
	}

	private def lineToAsteroids(lineWithIndex: (String, Int)): IndexedSeq[Asteroid] = {
		lineWithIndex._1
				.zipWithIndex
				.filter(charWithIndex => charWithIndex._1 == '#')
				.map(charWithIndex => new Asteroid(charWithIndex._2, lineWithIndex._2))
	}

}
