package pokemodel

import org.scalatest.FlatSpec

class MoveMakerSuite extends FlatSpec {

  "MoveMaker" should "create a new instance of a Move every time" in {
    val m1 = MoveMaker.makeMove(33)  // Tackle
    val m2 = MoveMaker.makeMove(33)  // Tackle
    assert (m1 != m2)
  }

  "MoveDepot" should "return the same instance of a Move every time" in {
    val m1 = MoveDepot(33)  // Tackle
    val m2 = MoveDepot(33)  // Tackle
    assert (m1 == m2)
  }

  it should "recognize strings: capitalization, hyphenation, and spacing ignored" in {
    assert (MoveDepot("tackle").index == 33)
    assert (MoveDepot("karate    CHOP").index == 2)
    assert (MoveDepot("PinMissile").index == 42)
    assert (MoveDepot("absorb").index == 71)
    assert (MoveDepot("Soft-Boiled").index == 135)
    assert (MoveDepot("Bubble Beam").index == 61)
  }

  it should "try adding 'Register' to the front of a move if it's not immediately found" in {
    assert (MoveDepot("solarbeam").index == 76)
  }
  

}
