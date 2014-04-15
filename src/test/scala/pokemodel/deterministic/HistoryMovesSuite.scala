package pokemodel

import org.scalatest._
import TestingInfrastructure._

/*
 * This file contains tests for the three moves that rely on former Battle
 * history: MirrorMove, Counter, and Bide.
 */

class HistoryMoveSuite extends FlatSpec {
  "MirrorMove" should "fail if used as the first Move of the Battle" in {
    val f = fullFixture(100, 80, List(MoveDepot("MirrorMove")), List(), "Spearow")
    import f._

    val result = p1.useMove(1, p2, battle)
    assert(result.numTimesHit == 0)
    assert(result.damageDealt == 0)
    assert(result.rawDamage == 0)
  }

  it should "fail if used after a new Pokemon has switched in" in {
    val f = megaFixture(
      List(("Fearow", 100)), 
      List(("Venusaur", 80), ("Machamp", 75)), 
      List(List("MirrorMove")), 
      List(List("RegisterSolarbeam", "VineWhip"), List("KarateChop", "SeismicToss"))
    )
    import f._

    // vals: team1/team2, trainer1/trainer2, battle
    assert(team2.activePokemon.level == 80)      // Venusaur active
    team2.useMove(2, team1, battle)              // use VineWhip against Fearow
    assert(team1.activePokemon.currentHP() > 0)  // Fearow survives
    team2.switch(2, battle)                      // bring out Machamp
    assert(team2.activePokemon.level == 75)      // Machamp active

    val result = team1.useMove(1, team2, battle) // use Mirror Move vs new Pokemon
    assert(result.numTimesHit == 0)
    assert(result.damageDealt == 0)
    assert(result.rawDamage == 0)
  }

  it should "succeed in the simplest case possible: DragonRage first, MM second" in {
    val f = fullFixture(100, 100, 
      List(MoveDepot("mirrormove")), List(MoveDepot("dragonrage")),
      "Fearow", "Dragonite")
    import f._

    // vals: team1/team2, trainer1/trainer2, battle
    team2.useMove(1, team1, battle)               // Dragonite uses DragonRage
    val result = team1.useMove(1, team2, battle)  // Fearow uses MirrorMove
    println(result)
    assert(p2.currentHP() < p2.maxHP)
  }


  "Counter" should "fail if no damage was previously dealt to the user" in {
    assert(1 == 0)
  }

  it should "fail if the previous damage dealt wasn't Normal- or Fighting-type damage" in {
    assert(1 == 0)
  }

  it should "deal twice the damage received if damage received was Normal-type" in {
    assert(1 == 0)
  }

  it should "deal twice the damage received if damage received was Fighting-type" in {
    assert(1 == 0)
  }

  it should "work against a Ghost-type Pokemon that deals Normal damage" in {
    assert(1 == 0)
  }

  it should "use the most recent attack when damaging" in {
    assert(1 == 0)
  }

  it should "deal the same amount of damage twice in a row if no damage is taken in between" in {
    assert(1 == 0)
  }

  it should "counter recoil damage if that's the most recent damage" in {
    assert(1 == 0)
  }

  it should "be unaffected by switching out and then back in" in {
    assert(1 == 0)
  }

  it should "be unaffected by using a multi-turn move if no damage taken in the meantime" in {
    assert(1 == 0)
  }

  it should "only use the last hit of a DoubleStrike attack" in {
    assert(1 == 0)
  }

  it should "only use the last hit of a MultiStrike attack" in {
    assert(1 == 0)
  }

  it should "deal the amount of damage the underlying Pokemon would have taken if damage wasn't absorbed by a sub" in {
    assert(1 == 0)
  }

  "Bide" should "have priority 1" in {
    val m = MoveDepot("Bide")
    assert(m.priority == 1)
  }
  
  it should "cause the user to be registered with the BideManager if not already registered" in {
    assert(false)
  }

  it should "cause a PP to be deducted immediately" in {
    assert(false)
  }
  
  it should "cause no more than 1 PP to be deducted if you continue moving towards attack" in {
    assert(false)
  }

  it should "deal back 2*40*(#DragonRages)" in {
    assert(false)
  }

  it should "be able to hit a Flying Pokemon" in {
    assert(false)
  }

  it should "be able to hit a Digging Pokemon" in {
    assert(false)
  }
}
