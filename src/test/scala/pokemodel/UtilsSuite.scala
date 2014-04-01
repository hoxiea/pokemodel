package pokemodel

import org.scalatest.FunSuite

import Type._

class UtilsSuite extends FunSuite {
  test("intBetween includes bottom, excludes top, and lands in between") {
    val lower = -6
    val upper = 6
    val x = Utils.intBetween(lower, upper)
    assert(x >= lower)
    assert(x < upper)
    assert(x != upper)
  }

  test("damageSeqCalc - a bunch of examples") {
    // Max damage all the way, since a*b < c
    assert(Utils.damageSeqCalc(4, 20, 100) == List(20, 20, 20, 20))
    assert(Utils.damageSeqCalc(3, 30, 100) == List(30, 30, 30))
    assert(Utils.damageSeqCalc(5, 15, 100) == List(15, 15, 15, 15, 15))

    // Each move can hit only once before killing defender, so just do one strike
    assert(Utils.damageSeqCalc(4, 20, 15) == List(15))
    assert(Utils.damageSeqCalc(3, 30, 25) == List(25))

    // Multiple strikes, and the last one does less than full damage
    assert(Utils.damageSeqCalc(4, 20, 50) == List(20, 20, 10))
    assert(Utils.damageSeqCalc(3, 30, 65) == List(30, 30, 5))
  }
}
