package pokemodel

import org.scalatest._
import TestingInfrastructure._

import Type._

/*
 * This file tests the logic of multi-turn moves.
 *
 * Examples: Dig, Fly, Skull Bash, Solar Beam, Rage, Thrash, etc.
 */

class MultiTurnMoveSuite extends FlatSpec {

  // DIG
  "RegisterDig" should "register the attacker with the Dig data structure" in {
    val f = fullFixture(100, 100, List(MoveDepot("registerdig")), List())  // Charizard can learn Dig
    import f._

    val result = p1.useMove(1, p2, battle)
    assert(battle.weirdMoveStatusManager.isDug(p1))
  }

  it should "not deduct a PP until DigAttack is used" in {
    val f = fullFixture(100, 100, List(MoveDepot("registerdig")), List())
    import f._

    p1.useMove(1, p2, battle)
    assert(p1.getPP(1).get == MoveDepot.maxPP("registerdig"))
  }

  it should "allow the Pokemon who registered to use Dig (Attack)" in {
    val f = fullFixture(100, 100, List(MoveDepot("registerdig")), List())
    import f._

    p1.useMove(1, p2, battle)
    val d = new Dig
    val result2 = d.use(p1, 5, p2, battle)  // no Exception!
  }

  "Dig" should "unregister a Dug Pokemon from being Dug" in {
    val f = fullFixture(100, 100, List(MoveDepot("registerdig")), List())
    import f._

    p1.useMove(1, p2, battle)
    val d = new Dig
    d.use(p1, 5, p2, battle)  
    assert(!battle.weirdMoveStatusManager.isDug(p1))
  }

  it should "deduct a PP from RegisterDig in" in {
    val f = fullFixture(100, 100, List(MoveDepot("registerdig")), List())
    import f._

    p1.useMove(1, p2, battle)
    new Dig().use(p1, 5, p2, battle)  
    assert(p1.getPP(1).get == MoveDepot.maxPP("registerdig") - 1)
  }
  
  it should "deal damage and correctly report that fact" in {
    val f = fullFixture(100, 100, List(MoveDepot("registerdig")), List())
    import f._

    p1.useMove(1, p2, battle) // register
    val result = new Dig().use(p1, 5, p2, battle)
    assert (result.moveType == Ground)
    assert (result.numTimesHit == 1)
    assert (result.damageCalc > 0)
    assert (result.damageDealt > 0)
    assert (p2.currentHP() < p2.maxHP)
  }

  "RegisterThrash" should "register the attacker with the Thrash data structure" in {
    val f = fullFixture(80, 75, List(MoveDepot("RegisterThrash")), List(), "Nidoking", "Mankey")
    import f._

    p1.useMove(1, p2, battle)
    assert(battle.weirdMoveStatusManager.isThrashing(p1))
  }
  
  it should "cause Thrash (attack) to be used, dealing damage" in {
    val f = fullFixture(80, 75, List(MoveDepot("RegisterThrash")), List(), "Nidoking", "Mankey")
    import f._

    val result = p1.useMove(1, p2, battle)
    assert(result.damageDealt > 0)
  }

  it should "cause the #turns remaining to drop" in {
    // TODO: This should be a WeirdMoveStatusManager test
    val f = fullFixture(80, 75, List(MoveDepot("RegisterThrash")), List(), "Nidoking", "Mankey")
    import f._

    assert(battle.weirdMoveStatusManager.getThrashTurnsLeft(p1).isEmpty) // not registered yet
    p1.useMove(1, p2, battle)  // register, use first Thrash
    val turnsAfter1 = battle.weirdMoveStatusManager.getThrashTurnsLeft(p1).get
    (new Thrash).use(p1, 1, p2, battle)  // don't use Register again, it'll fail
    val turnsAfter2 = battle.weirdMoveStatusManager.getThrashTurnsLeft(p1).get
    assert(turnsAfter1 > turnsAfter2 && turnsAfter2 > 0)
    println(battle.weirdMoveStatusManager.getThrashTurnsLeft(p1).get)
  }
  
}
