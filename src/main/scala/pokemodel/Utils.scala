package pokemodel

import scala.util.Random

object Utils {
  def intBetween(lower : Int, upper : Int) = {
    // Generate an Int between lower (inclusive) and upper (exclusive)
    require (lower <= upper, "invalid args to intBetween")
    lower + Random.nextInt(upper - lower)
  }

  /*
   * When doing a DoubleStrike or MultiStrike move, you first figure out:
   * - how much damage each strike is going to deal, using DamageCalculator
   * - how many times you're striking, either 2 or random
   *
   * But since Pokemon.takeDamage doesn't truncate excess damage anymore, AND
   * moves like Bide and Counter only use the last strike in the sequence,
   * we actually have to figure out the damage sequence that either deals
   * result.damageDealt damage all the way through, or stops short upon
   * killing opponent, breaking substitute, etc.
   *
   * This function figures out that sequence of damages.
   * There's a test for it in UtilsSuite that demonstrates various examples.
   */

  def damageSeqCalc(
    numStrikesLeft: Int,
    damageEachStrike: Int,
    defenderHPLeft: Int,
    soFar: List[Int] = List[Int]()): List[Int] = {

    if (numStrikesLeft == 0 || defenderHPLeft <= 0) soFar.reverse
    else {
      val damageToDeal = damageEachStrike min defenderHPLeft
      damageSeqCalc(numStrikesLeft - 1,
                    damageEachStrike,
                    defenderHPLeft - damageToDeal,
                    damageToDeal :: soFar)
    }
  }
}
