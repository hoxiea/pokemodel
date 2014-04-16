package pokemodel

import org.scalatest._
import TestingInfrastructure._

/*
 * This file contains deterministic tests for the following attacking Move traits:
 * - SingleStrike
 * - MultiStrike
 * - DoubleStrike
 * - ConstantDamage
 * - SuicideDamage
 * - OneHitKO
 * - TODO: SingleStrikeLoseHPOnMiss
 */

class AttackTraitSuite extends FlatSpec with Matchers {

  /* SINGLESTRIKE */
  "A SingleStrike move" should "deal the expected amount of damage with Power 40" in {
    val f = singleMoveFixture(new TestPhysicalSingleStrike with Power40)
    import f._
    val result = charizard.useMove(1, venusaur, battle)

    // used calculators for these values
    if (!result.critHit) {
      assert(29 <= result.damageDealt, s"reg damage ${result.damageDealt} too low")
      assert(result.damageDealt <= 35, s"reg damage ${result.damageDealt} too high")
    } else {
      assert(57 <= result.damageDealt, s"crithit damage ${result.damageDealt} too low")
      assert(result.damageDealt <= 68, s"crithit damage ${result.damageDealt} too high")
    }
  }

  it should "deal the expected amount of damage with Power 80" in {
    val f = singleMoveFixture(new TestPhysicalSingleStrike with Power80)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    // used calculator for these values
    if (!result.critHit) {
      assert(58 <= result.damageDealt, s"reg damage ${result.damageDealt} too low (58 min)")
      assert(result.damageDealt <= 69, s"reg damage ${result.damageDealt} too high (69 max)")
    } else {
      // TODO: calculator says 114?
      assert(113 <= result.damageDealt, s"crithit damage ${result.damageDealt} too low (114 min)")
      assert(result.damageDealt <= 134, s"crithit damage ${result.damageDealt} too high (134 max)")
    }
  }

  it should "deal the expected amount of damage with Power 120" in {
    val f = singleMoveFixture(new TestPhysicalSingleStrike with Power120)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    // used calculators for these values
    if (!result.critHit) {
      assert(87 <= result.damageDealt, "reg damage too low")
      assert(result.damageDealt <= 103, "reg damage too high")
    } else {
      assert(170 <= result.damageDealt, "crithit damage too low")
      assert(result.damageDealt <= 200, "crithit damage too high")
    }
  }

  it should "provide correct values to its MRB when it doesn't KO" in {
    val f = singleMoveFixture(new TestPhysicalSingleStrike with Power40)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.numTimesHit == 1, "numTimesHit")
    assert(result.STAB == false, "stab")
    assert(result.typeMult == 1.0, "typeMult")
    assert(result.nvsa.isEmpty, "nvsa")
    assert(result.vsa.isEmpty, "vsa")
    assert(result.KO == false, "KO")
    assert(result.selfKO == false, "selfKO")
    assert(venusaur.currentHP() == venusaur.maxHP - result.damageDealt, "opponent HP")
    assert(result.rawDamage == result.damageDealt, "case1")    // opponent is full health, no truncate
    assert(result.dUnderlying == result.damageDealt, "case2")  // no sub, so underlying damage same
  }

  it should "provide correct values to its MRB when it does KO via CritHit, STAB, and effectiveness" in {
    val f = fullFixture(100, 60,
        List(new TestPhysicalSingleStrike with Power120 with Fire with AlwaysCritHit), List())
    import f._
    val result = p1.useMove(1, p2, battle)

    // used calculators for these values
    // regular hit: 430 to 506
    // crit hit   : 839 to 986
    // So no matter what happens, Venusaur with 363 HP should die
    assert(result.damageDealt == p2.maxHP, "damageDealt didn't equal Venusaur's maxHP")
    assert(result.STAB == true, "stab")  // Charizard is Type1 Fire
    assert(result.typeMult == 2.0, "typeMult")  // Fire is super effective against Plant
    assert(result.nvsa.isEmpty, "nvsa")
    assert(result.vsa.isEmpty, "vsa")
    assert(result.KO == true, "KO")  // venusaur has 222 HP
    assert(result.selfKO == false, "selfKO")
    assert(result.rawDamage > result.damageDealt, "case1")     // truncate
    assert(result.dUnderlying == result.damageDealt, "case2")  // no sub, so underlying damage same
  }


  it should "provide correct values to its MRB when the sub present isn't KOed" in {
    val f = subFixture(new TestPhysicalSingleStrike with Power40)
    import f._
    // Venusaur has 10HP, VenusaurSub has 91P
    val result = charizard.useMove(1, venusaur, battle)  // 29-35 or 57-68 damage, no subKO
    assert(result.numTimesHit == 1, "numTimesHit")
    assert(result.STAB == false, "stab")
    assert(result.typeMult == 1.0, "typeMult")
    assert(result.nvsa.isEmpty, "nvsa")
    assert(result.vsa.isEmpty, "vsa")
    assert(result.KO == false, "KO")
    assert(result.selfKO == false, "selfKO")
    assert(venusaur.currentHP(true) == 10, "underlying HP")
    assert(venusaur.currentHP(false) == 91 - result.damageDealt, "sub HP")
    assert(result.rawDamage == result.damageDealt, "case1")  // all raw damage dealt to sub, no KO
    assert(result.dUnderlying == 10, "case2")    // would have dealt 10 damage if sub not present
  }

  it should "provide correct values to its MRB when the sub present is KOed" in {
    val f = subFixture(new TestPhysicalSingleStrike with Power40)
    import f._
    reduceHPTo(venusaur, 20)
    assert(venusaur.currentHP() == 20)
    assert(venusaur.currentHP(true) == 10)

    val result = charizard.useMove(1, venusaur, battle)  // 29-35 or 57-68 damage => subKO
    assert(result.numTimesHit == 1, "numTimesHit")
    assert(result.KO == false, "KO")
    assert(result.selfKO == false, "selfKO")
    assert(result.subKO, "subKO")
    assert(!venusaur.hasSub, "hasSub")
    assert(venusaur.currentHP() == 10, "currentHP 2")
    assert(venusaur.currentHP(true) == 10, "currentHP 1")  // no sub to bypass => Pokemon HP
    assert(result.rawDamage > result.damageDealt, "case1")  // only dealt 20 damage
    assert(result.dUnderlying == 10, "case2")    // would have dealt 10 damage if sub not present
  }

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
    val hpLost = venusaur.maxHP - venusaur.currentHP()
    assert(result.damageDealt < hpLost)
  }

  it should "register only the damage dealt on the last blow in dUnderlying" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    val hpLost = venusaur.maxHP - venusaur.currentHP()
    assert(result.dUnderlying < hpLost)
  }

  it should "deal total damage as given by totalDamageDealt on a full-health enemy" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    val hpLost = venusaur.maxHP - venusaur.currentHP()
    assert(hpLost == totalDamageDealt(result))
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
    val result = charizard.useMove(1, venusaur, battle)
    assert(venusaur.maxHP - venusaur.currentHP() == totalDamageDealt(result))
  }

  it should "only record one strike if it kills the opponent with the first strike" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike)
    import f._
    val enemyHP = 10
    reduceHPTo(venusaur, enemyHP)
    val result = charizard.useMove(1, venusaur, battle)  // now use the move
    assert(result.numTimesHit == 1)
    assert(result.damageDealt == 10)
    assert(result.dUnderlying == 10)
  }

  it should "stop if it kills a substitute without hurting the underlying Pokemon" in {
    val f = subFixture(new TestPhysicalMultiStrike with Power120)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.numTimesHit <= 2)     // Power120 always kills in 1 or 2 strikes
    assert(!result.KO)                  // MR says underlying venusaur is fine
    assert(venusaur.currentHP() == 10)  // Underlying venusaur is fine
    assert(result.subKO)       // MP says sub is dead
    assert(!venusaur.hasSub)   // Sub is dead
  }

  it should "correctly record damageDealt and dUnderlying if it breaks a sub on first hit" in {
    val f = subFixture(new TestPhysicalMultiStrike with Power200 with Fire)
    import f._
    // venusaur has 10HP, vsub has 91HP
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.numTimesHit == 1)
    assert(!result.KO)                  // MR says underlying venusaur is fine
    assert(venusaur.currentHP() == 10)  // Underlying venusaur is fine
    assert(result.subKO)       // MP says sub is dead
    assert(!venusaur.hasSub)   // Sub is dead
    assert(result.dUnderlying == 10)
    assert(result.damageDealt == 91)
  }

  it should "get MRB damages right if it breaks a sub but wouldn't have killed underlying Pokemon" in {
    val f = subFixture(new TestPhysicalMultiStrike with Power40)
    import f._
    venusaur.gainHP(353)  // back to full health, 363HP, plus 91HP sub
    venusaur.takeDamage(81)  // drop sub to 10 HP
    assert(venusaur.currentHP() == 10)
    assert(venusaur.currentHP(true) == 363)

    val result = charizard.useMove(1, venusaur, battle)
    assert(result.numTimesHit == 1)
    assert(!result.KO)                  // MR says underlying venusaur is fine
    assert(venusaur.currentHP(true) == 363)  // Underlying venusaur is fine
    assert(result.subKO)       // MP says sub is dead
    assert(!venusaur.hasSub)   // Sub is dead
    assert(result.dUnderlying == result.rawDamage)
    assert(result.damageDealt == 10)
  }

  it should "get MRB damages right if it would have to truncate for underlying Pokemon but not for sub" in {
    val f = subFixture(new TestPhysicalMultiStrike with Power20 with NeverCritHit)
    // according to calculator, 15-18 damage per strike
    // this would have to truncate for 10HP underlying Venusaur, but 18*5 = 90 should never kill sub
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(15 <= result.rawDamage && result.rawDamage <= 18)
    assert(!result.KO)                   // MR says underlying venusaur is fine
    assert(venusaur.currentHP(true) > 0) // Underlying venusaur is fine
    assert(!result.subKO)       // MP says sub survives
    assert(venusaur.hasSub)     // Sub survived
    assert(result.dUnderlying == 10)
    assert(result.damageDealt == result.rawDamage)
  }

  it should "correctly record STAB and type effectiveness when super effective" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike with Power120 with Fire)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.STAB)
    assert(result.typeMult == 2.0)
  }

  it should "correctly record STAB and type effectiveness when not very effective" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike with Power120 with Grass)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(!result.STAB)
    assert(result.typeMult == 0.25)  // Grass is half against both Grass and Poison
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
    assert(venusaur.maxHP - venusaur.currentHP() == result.rawDamage * 2)
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
    val f = subFixture(new TestSpecialDoubleStrike with Power200)
    import f._
    val result = charizard.useMove(1, venusaur, battle)  // now use the move
    assert(result.numTimesHit == 1)     // Power200 always kills in 1 strike
    assert(venusaur.currentHP() == 10)  // Underlying venusaur is fine
  }

  it should "correctly record STAB and type effectiveness when super effective" in {
    val f = singleMoveFixture(new TestSpecialDoubleStrike with Fire)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.STAB)
    assert(result.typeMult == 2.0)
  }

  it should "correctly record STAB and type effectiveness when not very effective" in {
    val f = singleMoveFixture(new TestSpecialDoubleStrike with Fighting)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(!result.STAB)
    assert(result.typeMult == 0.5)  // Fighting is 0.5 against Poison and 1.0 against Grass
  }


  /* CONSTANT DAMAGE */
  "A ConstantDamage Move" should "cause HP to drop by its damageAmount when enemy is full-health" in {
    val f = singleMoveFixture(MoveDepot("dragonrage"))
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.damageDealt == 40, "damageDealt")
    assert(result.STAB == false, "stab")
    assert(result.typeMult == 1.0, "typeMult")
    assert(result.nvsa.isEmpty, "nvsa")
    assert(result.vsa.isEmpty, "vsa")
    assert(result.KO == false, "KO")
    assert(result.selfKO == false, "selfKO")
  }

  it should "should kill opponent if opponent has <=damageAmount HP" in {
    val f = singleMoveFixture(MoveDepot("dragonrage"))
    import f._
    reduceHPTo(venusaur, 30)
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.KO, "KO")
    assert(result.rawDamage == 40)
    assert(result.damageDealt == 30)
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

  it should "deal typed damage, i.e. deal no damage against Ghost as type Normal" in {
    val f = fullFixture(100, 100, List(new TestSD with Power40), List(), "Electrode", "Gengar")
    import f._
    val result = p1.useMove(1, p2, battle)
    assert(!result.subKO, "subKO")
    assert(!result.KO, "KO")
    assert(result.rawDamage == 0, "dCalc")  // Damage Calculator takes types into effect
    assert(result.damageDealt == 0, "dDealt")
    assert(result.selfKO, "selfKO")
    assert(p2.currentHP() == p2.maxHP, "enemy full health")
  }

  /* OneHitKO */
  "A OneHitKO move" should "always kill the enemy if it hits" in {
    val f = singleMoveFixture(new TestOneHitKO)  // 1.0 accuracy
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.damageDealt == venusaur.maxHP, "damageDealt error")
    assert(result.numTimesHit == 1)
    assert(result.KO)
    assert(!result.selfKO)
  }

  it should "break a substitute if one exists and it hits" in {
    val f = subFixture(new TestOneHitKO)  // 1.0 accuracy
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.damageDealt == venusaur.maxHP/4 + 1, "damageDealt error")
    assert(result.numTimesHit == 1)
    assert(!result.KO)
    assert(!result.selfKO)
    assert(result.subKO)
  }

  it should "always fail if the attacker is slower than the defender" in {
    val f = singleMoveFixture(new TestOneHitKO)  // 1.0 accuracy
    import f._
    battle.statManager.setSpeedStage(charizard, -6)  // slooooooooowwwwww
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.rawDamage == 0)  // never even consult the DamageCalculator
    assert(result.damageDealt == 0)
    assert(result.numTimesHit == 0)
    assert(!result.KO)
    assert(!result.selfKO)
    assert(!result.subKO)
  }

}
