package pokemodel

import org.scalatest._
import TestingInfrastructure._

class HPGainTraitSuite extends FlatSpec with Matchers {

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
    assert(result.rawDamage > 0)
    assert(result.hpGained == 0, "(health gained)")
    assert(charizard.currentHP() == charizard.maxHP)
  }

  it should "restore HP if you connect and started with less than full HP" in {
    val f = singleMoveFixture(new TestGPDD with Power80)
    import f._
    charizard.takeDamage(20)
    val result = charizard.useMove(1, venusaur, battle)
    result.numTimesHit should be (1)
    assert(result.rawDamage > 0)
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
    assert(result.rawDamage == 0, "rawDamage")  // SingleStrike fails, no DC involved
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
    assert(result.rawDamage > 0, "rawDamage")  // SingleStrike fails, no DC involved
    assert(result.damageDealt > 0, "damageDealt")
    assert(result.hpGained == result.damageDealt / 2, "hpGained")
    assert(p1.currentHP() > startingHP, "hp actually restored")
  }


  // TODO: Test Recover/Softboiled trait

}
