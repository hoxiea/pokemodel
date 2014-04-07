package pokemodel

import org.scalatest._
import TestingInfrastructure._

class MoveFlatSpec extends FlatSpec with Matchers {

  /* MULTISTRIKE */
  "A Multistrike move" should "hit between 2-5 times" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(2 <= result.numTimesHit && result.numTimesHit <= 5)
  }

  it should "register only the damage dealt on the last blow in damageDealt" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.damageDealt <= result.damageCalc)
  }

  it should "deal total damage as given by totalDamageDealt on a full-health enemy" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(venusaur.maxHP - venusaur.currentHP() == totalDamageDealt(result))
  }

  it should "deal total damage as given by totalDamageDealt on an almost-dead enemy" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike)
    import f._
    val enemyHP = 30
    reduceHPTo(venusaur, enemyHP)
    val result = charizard.useMove(1, venusaur, battle)  // now use the move
    assert(totalDamageDealt(result) == enemyHP)
  }

  it should "use the damage from a Critical Hit for all strikes" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike with AlwaysCritHit)
    import f._
    val result = charizard.useMove(1, venusaur, battle)  // now use the move
    assert(venusaur.maxHP - venusaur.currentHP() == totalDamageDealt(result))
  }

  it should "only record one strike if it kills the opponent with the first strike" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike)
    import f._
    val enemyHP = 10
    reduceHPTo(venusaur, enemyHP)
    val result = charizard.useMove(1, venusaur, battle)  // now use the move
    assert(result.numTimesHit == 1)
  }

  it should "stop if it kills a substitute without hurting the underlying Pokemon" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike with Power120)
    import f._
    val maxHP = 363   // printed out Venusaur once to check
    assert(venusaur.maxHP == maxHP)

    // 363/4 == 90 == cost of making a substitute
    // So get it to 100 HP, then make sub => sub has 91 HP, venusaur has 10 HP
    val enemyHP = 100
    reduceHPTo(venusaur, enemyHP)
    venusaur.makeSub()
    assert(venusaur.hasSub)
    assert(venusaur.currentHP(false) == (maxHP / 4) + 1)
    assert(venusaur.currentHP(true) == enemyHP - (maxHP / 4))

    val result = charizard.useMove(1, venusaur, battle)  // now use the move

    assert(result.numTimesHit <= 2)     // Power120 always kills in 1 or 2 strikes
    assert(!result.KO)                  // Underlying venusaur is fine
    assert(venusaur.currentHP() == 10)  // Underlying venusaur is fine
    assert(result.subKO)       // Sub is dead
    assert(!venusaur.hasSub)   // Sub is reset
  }


  /* DOUBLESTRIKE */
  "A DoubleStrike move" should "hit a full-HP enemy exactly 2 times" in {
    val f = singleMoveFixture(new TestSpecialDoubleStrike)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(2 == result.numTimesHit)
  }

  it should "deal total damage as given by totalDamageDealt on a full-health enemy" in {
    val f = singleMoveFixture(new TestSpecialDoubleStrike)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(venusaur.maxHP - venusaur.currentHP() == totalDamageDealt(result))
  }

  it should "hit a low-HP enemy exactly 1 times" in {
    val f = singleMoveFixture(new TestSpecialDoubleStrike)
    import f._

    val enemyHP = 10
    venusaur.takeDamage(venusaur.maxHP - enemyHP)  // get it down to 10 HP
    assert(venusaur.currentHP() == enemyHP)

    val result = charizard.useMove(1, venusaur, battle)
    assert(1 == result.numTimesHit)
  }

  it should "deal total damage as given by totalDamageDealt on a low-health enemy" in {
    val f = singleMoveFixture(new TestSpecialDoubleStrike)
    import f._

    val enemyHP = 10
    venusaur.takeDamage(venusaur.maxHP - enemyHP)  // get it down to 10 HP
    assert(venusaur.currentHP() == enemyHP)

    val result = charizard.useMove(1, venusaur, battle)
    assert(enemyHP == totalDamageDealt(result))
  }

  it should "use the damage from a Critical Hit for both strikes" in {
    val f = singleMoveFixture(new TestSpecialDoubleStrike with AlwaysCritHit)
    import f._
    val result = charizard.useMove(1, venusaur, battle)  // now use the move
    assert(venusaur.maxHP - venusaur.currentHP() == result.damageCalc * 2)
  }

  it should "stop after 1 strike if it breaks a substitute" in {
    val f = subFixture(new TestSpecialDoubleStrike with Power200)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.numTimesHit == 1)   // Power200 always kills in 1 strike
    assert(!result.KO)                // Underlying venusaur is fine
    assert(result.subKO)       // Sub is dead
    assert(!venusaur.hasSub)   // Sub was reset
  }

  it should "not damage the underlying Pokemon if it breaks a substitute on Strike 1" in {
    val f = subFixture(new TestPhysicalMultiStrike with Power200)
    import f._
    val result = charizard.useMove(1, venusaur, battle)  // now use the move
    assert(result.numTimesHit == 1)     // Power200 always kills in 1 strike
    assert(venusaur.currentHP() == 10)  // Underlying venusaur is fine
  }

  /* SUICIDEDAMAGE */
  "A SuicideDamage move" should "hit once if accuracy = 1.0 and no accuracy adjustments" in {
    val f = singleMoveFixture(new TestSD)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.numTimesHit == 1)
  }

  it should "cause a selfKO if there's no sub (and therefore no glitch to save it)" in {
    val f = singleMoveFixture(new TestSD)
    import f._
    assert(!venusaur.hasSub)
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.selfKO)
  }

  it should "not cause a selfKO if Glitch.suicideGlitchOn and using it breaks a sub" in {
    val f = subFixture(new TestSD)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.subKO)
    assert(!result.selfKO)   // he lives, it's a miracle/glitch!
  }

  it should "cause a selfKO if Glitch.suicideGlitchOn and using it ONLY DAMAGES a sub" in {
    val f = subFixture(new TestSD with Power40)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(!result.subKO)   // Power40 means it's not strong enough to deal >= 91 damage
    assert(result.selfKO)
  }

  /* GAIN PROP DAMAGE DEALT */
  "A GainPropDamageDealt move" should "throw an Exception if no trait mixed in before" in {
    val f = singleMoveFixture(new TestGPDDWRONG)
    import f._
    an [Exception] should be thrownBy {
      val result = charizard.useMove(1, venusaur, battle)
    }
  }

  it should "restore no HP if you connect but started at full HP" in {
    val f = singleMoveFixture(new TestGPDD)
    import f._
    assert(charizard.currentHP() == charizard.maxHP)
    val result = charizard.useMove(1, venusaur, battle)
    result.numTimesHit should be (1)
    assert(result.damageCalc > 0)
    assert(result.hpGained == 0, "(health gained)")
    assert(charizard.currentHP() == charizard.maxHP)
  }

  it should "restore HP if you connect and started with less than full HP" in {
    val f = singleMoveFixture(new TestGPDD with Power80)
    import f._
    charizard.takeDamage(20)
    val result = charizard.useMove(1, venusaur, battle)
    result.numTimesHit should be (1)
    assert(result.damageCalc > 0)
    assert(result.hpGained == 20, "(health gained)")
    assert(charizard.currentHP() == charizard.maxHP)
  }

  it should "restore HP in proportion to how much damage was dealt" in {
    val f = singleMoveFixture(new TestGPDD with Power80)
    import f._
    val startingHP = 50
    reduceHPTo(charizard, startingHP)
    val result = charizard.useMove(1, venusaur, battle)
    result.numTimesHit should be (1)
    assert(result.damageDealt > 0)
    assert(result.hpGained == result.damageDealt / 2, "(health gained)")
    assert(charizard.currentHP() == startingHP + result.hpGained)
  }

  it should "restore 1 HP if 1 damage was dealt" in {
    val f = singleMoveFixture(new TestGPDDConstant)
    import f._
    val startingHP = 50
    reduceHPTo(charizard, startingHP)
    val result = charizard.useMove(1, venusaur, battle)
    result.numTimesHit should be (1)
    assert(result.damageDealt == 1)
    assert(result.hpGained == 1, "(health gained)")
    assert(charizard.currentHP() == startingHP + result.hpGained)
  }

  it should "restore 0 HP if the attack broke a substitute" in {
    val f = subFixture(new TestGPDD with Power200)
    import f._

    val startingHP = 50
    reduceHPTo(charizard, startingHP)
    val result = charizard.useMove(1, venusaur, battle)

    result.numTimesHit should be (1)
    assert(result.subKO, "(subKO)")
    assert(result.hpGained == 0, "(health gained)")
    assert(charizard.currentHP() == startingHP)
  }

  /* DREAMEATER - A WEIRD PROPGAIN MOVE */
  "Dreameater" should "fail if the enemy isn't asleep" in {
    val f = fullFixture(100, 100, List(MoveDepot("dream eater")), List(), "Hypno")
    import f._

    val startingHP = 50
    reduceHPTo(p1, startingHP)
    val result = p1.useMove(1, p2, battle)  // Hypno uses dreameater

    assert(result.numTimesHit == 0, "numTimesHit")
    assert(result.damageCalc == 0, "damageCalc")  // SingleStrike fails, no DC involved
    assert(result.damageDealt == 0, "damageDealt")
    assert(result.hpGained == 0, "hpGained")
  }

  it should "deal damage and restore HP if the enemy is asleep" in {
    val f = fullFixture(100, 100, List(MoveDepot("dream eater")), List(), "Hypno")
    import f._

    // Put Hypno in a place where he can gain HP
    val startingHP = 50
    reduceHPTo(p1, startingHP)

    // Put p2 to sleep
    val sleeping = battle.statusManager.changeMajorStatusAilment(p2, SLP())
    assert(sleeping)
    assert(p2.isAsleep)

    val result = p1.useMove(1, p2, battle)  // Hypno uses dreameater
    assert(result.numTimesHit == 1, "numTimesHit")
    assert(result.damageCalc > 0, "damageCalc")  // SingleStrike fails, no DC involved
    assert(result.damageDealt > 0, "damageDealt")
    assert(result.hpGained == result.damageDealt / 2, "hpGained")
    assert(p1.currentHP() > startingHP, "hp actually restored")
  }
}
