package pokemodel

import org.scalatest.FunSuite

class MoveMakerSuite extends FunSuite {
  test("MoveMaker creates a new instance of a Move every time") {
    val m1 = MoveMaker.makeMove(33)  // Tackle
    val m2 = MoveMaker.makeMove(33)  // Tackle
    assert (m1 != m2)
  }
}
