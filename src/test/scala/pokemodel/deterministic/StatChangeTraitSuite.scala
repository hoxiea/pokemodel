package pokemodel

import org.scalatest._
import TestingInfrastructure._

class MoveFlatSpec extends FlatSpec with Matchers {

  /* SelfStatChange */
  "A SelfStatChange move" should "register the stat change with the StatManager (A|D|Spd)" in {
    val f = fullFixture(100, 100, 
      List(new TestIncreaseSelfAttackStat, new TestIncreaseSelfDefenseStat,
           new TestIncreaseSelfSpeedStat), List())
    import f._
    p1.useMove(1, p2, battle)
    p1.useMove(2, p2, battle)
    p1.useMove(3, p2, battle)
    assert(battle.statManager.attackStages(p1) == 1, "attack")
    assert(battle.statManager.defenseStages(p1) == 2, "defense")
    assert(battle.statManager.speedStages(p1) == 1, "speed")
  }

  it should "register the stat change with the StatManager (Spcl|Acc|Eva)" in {
    val f = fullFixture(100, 100, 
      List(new TestIncreaseSelfSpecialStat, new TestIncreaseSelfAccuracyStat,
           new TestIncreaseSelfEvasionStat), List())
    import f._
    p1.useMove(1, p2, battle)
    p1.useMove(2, p2, battle)
    p1.useMove(3, p2, battle)
    assert(battle.statManager.specialStages(p1) == 2, "special")
    assert(battle.statManager.accuracyStages(p1) == 1, "accuracy")
    assert(battle.statManager.evasionStages(p1) == 2, "evasion")
  }

  /* EnemyStatChange */
  "An EnemyStatChange move (soloStatChange=true)" should "register the stat change w/ StatManager (A|D|Spd)" in {
    val f = fullFixture(100, 100, 
      List(new TestDecreaseEnemyAttack, new TestDecreaseEnemyDefense,
           new TestDecreaseEnemySpeed), List())
    import f._
    p1.useMove(1, p2, battle)
    p1.useMove(2, p2, battle)
    p1.useMove(3, p2, battle)
    assert(battle.statManager.attackStages(p2) == -1, "attack")
    assert(battle.statManager.defenseStages(p2) == -2, "defense")
    assert(battle.statManager.speedStages(p2) == -1, "speed")
  }

  it should "register the stat change with the StatManager (Spcl|Acc|Eva)" in {
    val f = fullFixture(100, 100, 
      List(new TestDecreaseEnemySpecial, new TestDecreaseEnemyAccuracy,
           new TestDecreaseEnemyEvasion), List())
    import f._
    p1.useMove(1, p2, battle)
    p1.useMove(2, p2, battle)
    p1.useMove(3, p2, battle)
    assert(battle.statManager.specialStages(p2) == -2, "special")
    assert(battle.statManager.accuracyStages(p2) == -1, "accuracy")
    assert(battle.statManager.evasionStages(p2) == -2, "evasion")
  }

  it should "cause stat changes to stack (but not past -6)" in {
    val f = singleMoveFixture(new TestDecreaseEnemySpecial)
    import f._
    charizard.useMove(1, venusaur, battle)
    assert(battle.statManager.specialStages(venusaur) == -2, "special")
    charizard.useMove(1, venusaur, battle)
    assert(battle.statManager.specialStages(venusaur) == -4, "special")
    charizard.useMove(1, venusaur, battle)
    assert(battle.statManager.specialStages(venusaur) == -6, "special")
    charizard.useMove(1, venusaur, battle)
    assert(battle.statManager.specialStages(venusaur) == -6, "special")
  }

}
