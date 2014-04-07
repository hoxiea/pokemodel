package pokemodel

import org.scalatest.FunSuite

class MoveMakerSuite extends FunSuite {
  test("MoveMaker creates a new instance of a Move every time") {
    val m1 = MoveMaker.makeMove(33)  // Tackle
    val m2 = MoveMaker.makeMove(33)  // Tackle
    assert (m1 != m2)
  }

  test("MoveDepot returns the same instance of a Move every time") {
    val m1 = MoveDepot(33)  // Tackle
    val m2 = MoveDepot(33)  // Tackle
    assert (m1 == m2)
  }

  test("MoveDepot recognizes strings: capitalization, hyphenation, and spacing ignored") {
    assert (MoveDepot("tackle").index == 33)
    assert (MoveDepot("karate    CHOP").index == 2)
    assert (MoveDepot("PinMissile").index == 42)
    assert (MoveDepot("absorb").index == 71)
    assert (MoveDepot("Soft-Boiled").index == 135)
    assert (MoveDepot("Bubble Beam").index == 61)
  }
}
