package pokemodel

import org.scalatest._
import TestingInfrastructure._
import Type._

import org.scalacheck._

/*
 * This file contains deterministic tests for the three moves that rely on
 * former Battle history: MirrorMove, Counter, and Bide.
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
    assert(p2.currentHP() < p2.maxHP)
  }

  private def counterFailed(mr: MoveResult): Boolean = {
    mr.numTimesHit == 0 &&
    mr.rawDamage == 0 &&
    mr.damageDealt == 0 &&
    mr.dUnderlying == 0
  }

  "Counter" should "fail if no damage was previously dealt to the user" in {
    val f = fullFixture(100, 100,
      List(MoveDepot("counter")),
      List(MoveDepot("Growl"), MoveDepot("SwordsDance"), MoveDepot("Growth"))
    )
    import f._

    // use a bunch of non-damaging moves
    p2.useMove(1, p1, battle)
    p2.useMove(2, p1, battle)
    p2.useMove(3, p1, battle)

    val result = p1.useMove(1, p2, battle) // use Counter
    assert(counterFailed(result))
  }

  it should "fail if the previous damage dealt wasn't Normal- or Fighting-type damage" in {
    val f = fullFixture(100, 100, List(MoveDepot("counter")), List(MoveDepot("VineWhip")))
    import f._
    p2.useMove(1, p1, battle)
    assert(battle.counterMan.lastDamageDealtMR(p1).isDefined) // has a damage history, but wrong type
    val result = p1.useMove(1, p2, battle) // use Counter
    assert(counterFailed(result))
  }

  it should "deal twice the damage received if damage received was Normal-type" in {
    val f = fullFixture(100, 100, List(MoveDepot("counter")), List(MoveDepot("tackle")))
    import f._
    val tackleResult = p2.useMove(1, p1, battle)
    assert(battle.counterMan.lastDamageDealtMR(p1).isDefined)
    assert(battle.counterMan.lastDamageDealtMR(p1).get == tackleResult)
    assert(tackleResult.moveType == Normal)

    val result = p1.useMove(1, p2, battle) // use Counter
    assert(result.numTimesHit == 1)
    assert(result.damageDealt == 2 * tackleResult.damageDealt)
    assert(p2.currentHP() < p2.maxHP)
  }

  it should "deal twice the damage received if damage received was Fighting-type" in {
    val f = fullFixture(100, 100,
      List(MoveDepot("counter")), List(MoveDepot("seismictoss")),
      "Charizard", "Mankey")
    import f._

    val stResult = p2.useMove(1, p1, battle)
    assert(battle.counterMan.lastDamageDealtMR(p1).isDefined)
    assert(battle.counterMan.lastDamageDealtMR(p1).get == stResult)
    assert(stResult.moveType == Fighting)

    val result = p1.useMove(1, p2, battle) // use Counter
    assert(result.numTimesHit == 1)
    assert(result.damageDealt == 2*stResult.damageDealt, "Counter damageDealt")
    assert(p2.currentHP() < p2.maxHP)
  }

  it should "work against Struggle" in {
    val f = singleMoveFixture(MoveDepot("counter"))
    import f._

    val struggleRes = venusaur.useMove(5, charizard, battle)
    assert(battle.counterMan.lastDamageDealtMR(charizard).isDefined)
    assert(battle.counterMan.lastDamageDealtMR(charizard).get == struggleRes)

    val counterRes = charizard.useMove(1, venusaur, battle)
    assert(counterRes.damageDealt > 0)
    assert(counterRes.numTimesHit == 1)
  }

  it should "work against HiJumpKick" in {
    val f = fullFixture(100, 100,
      List(MoveDepot("counter")), List(MoveDepot("hijumpkick")),
      "Charizard", "Hitmonlee")
    import f._

    val hjRes = p2.useMove(1, p1, battle)
    assert(battle.counterMan.lastDamageDealtMR(p1).isDefined)
    assert(battle.counterMan.lastDamageDealtMR(p1).get == hjRes)

    val counterRes = p1.useMove(1, p2, battle)
    assert(counterRes.rawDamage == hjRes.damageDealt * 2)
    assert(counterRes.rawDamage >= counterRes.damageDealt)
    assert(counterRes.numTimesHit == 1)
  }

  it should "work against Strength" in {
    val f = fullFixture(100, 100,
      List(MoveDepot("counter")), List(MoveDepot("Strength")),
      "Charizard", "Rhyhorn")
    import f._

    val res = p2.useMove(1, p1, battle)
    assert(battle.counterMan.lastDamageDealtMR(p1).isDefined)
    assert(battle.counterMan.lastDamageDealtMR(p1).get == res)

    val counterRes = p1.useMove(1, p2, battle)
    assert(counterRes.rawDamage == res.damageDealt * 2)
    assert(counterRes.rawDamage >= counterRes.damageDealt)
    assert(counterRes.numTimesHit == 1)
  }

  it should "get its MRB damage values correct if defender has low HP" in {
    val f = fullFixture(100, 100,
      List(MoveDepot("counter")), List(MoveDepot("Strength")),
      "Charizard", "Rhyhorn")
    import f._
    val enemyHP = 20
    reduceHPTo(p2, enemyHP)

    val res = p2.useMove(1, p1, battle)
    val counterRes = p1.useMove(1, p2, battle)
    assert(counterRes.numTimesHit == 1)
    assert(counterRes.rawDamage == res.damageDealt * 2)
    assert(counterRes.damageDealt == enemyHP)
    assert(counterRes.dUnderlying == enemyHP)
  }

  it should "get its MRB damage values correct if defender's sub is broken" in {
    val f = subFixture(MoveDepot("counter"))
    import f._
    val pokHP = 10
    val subHP = 20
    reduceHPTo(venusaur, subHP)  // actually detracts from sub
    assert(venusaur.currentHP() == subHP)
    assert(venusaur.currentHP(true) == pokHP)  // from subFixture

    val res = venusaur.useMove(2, charizard, battle)  // move2 is tackle
    val counterRes = charizard.useMove(1, venusaur, battle)
    assert(counterRes.numTimesHit == 1)
    assert(counterRes.subKO)
    assert(!counterRes.KO)
    assert(counterRes.rawDamage == res.damageDealt * 2)
    assert(counterRes.damageDealt == subHP)
    assert(counterRes.dUnderlying == pokHP)
  }

  it should "get its MRB damage values correct if defender's sub isn't broken" in {
    val f = subFixture(MoveDepot("counter"))
    import f._

    // hit charizard with 20HP of Normal damage
    val res = MoveDepot("sonicboom").use(venusaur, 5, charizard, battle)
    val counterRes = charizard.useMove(1, venusaur, battle)
    assert(counterRes.numTimesHit == 1)
    assert(!counterRes.subKO, "subKO")
    assert(!counterRes.KO, "KO")
    assert(counterRes.rawDamage == res.damageDealt * 2, "test1")
    assert(counterRes.damageDealt == counterRes.rawDamage, "test2")
    assert(counterRes.dUnderlying == 10, "test3")
  }

  it should "deal full damage against a Ghost-type Pokemon" in {
    // The Ghost doesn't actually have to deal the last damage
    val f = fullFixture(100, 100,
      List(MoveDepot("counter")), List(),
      "Charizard", "Gastly")
    import f._

    // hit charizard with 20HP of Normal damage
    val res = MoveDepot("sonicboom").use(p2, 5, p1, battle)
    val counterRes = p1.useMove(1, p2, battle)

    assert(counterRes.numTimesHit == 1)
    assert(counterRes.rawDamage == res.damageDealt * 2, "test1")
    assert(counterRes.damageDealt == counterRes.rawDamage, "test2")
    assert(counterRes.damageDealt > 0, "test3")
    assert(p2.currentHP() < p2.maxHP, "test4")
  }

  it should "use the most recent attack when damaging" in {
    val f = singleMoveFixture(MoveDepot("counter"))
    import f._

    MoveDepot("sonicboom").use(venusaur, 5, charizard, battle)
    val res2 = MoveDepot("tackle").use(venusaur, 5, charizard, battle)
    val counterRes = charizard.useMove(1, venusaur, battle)

    // make sure the tackle is the one that's countered
    assert(counterRes.numTimesHit == 1)
    assert(counterRes.rawDamage == res2.damageDealt * 2, "test1")
    assert(counterRes.damageDealt > 0, "test2")
    assert(venusaur.currentHP() < venusaur.maxHP, "test3")
  }

  it should "deal the same amount of damage twice in a row if no damage is taken in between" in {
    val f = singleMoveFixture(MoveDepot("counter"))
    import f._

    MoveDepot("sonicboom").use(venusaur, 5, charizard, battle)
    val counterRes1 = charizard.useMove(1, venusaur, battle)
    val intermedHP = venusaur.currentHP()
    val counterRes2 = charizard.useMove(1, venusaur, battle)
    assert(counterRes1.rawDamage == counterRes2.rawDamage, "test1")
    assert(counterRes1.damageDealt == counterRes2.damageDealt, "test2")
    assert(intermedHP < venusaur.maxHP, "test3")
    assert(intermedHP > venusaur.currentHP(), "test4")
  }

  it should "counter recoil damage if that's the most recent damage TAKEN" in {
    val f = singleMoveFixture(MoveDepot("counter"))
    import f._

    val venResult = charizard.useMove(5, venusaur, battle)  // Charizard struggles
    val intermedHP = venusaur.currentHP()
    assert(battle.counterMan.lastDamageDealtMR(charizard).isDefined)
    val charResult = battle.counterMan.lastDamageDealtMR(charizard).get  // Struggle MR
    assert(charResult.damageDealt == (venResult.damageDealt * 0.5).toInt)

    // Charizard can then Counter the recoil damage it took
    val counterRes = charizard.useMove(1, venusaur, battle)
    assert(counterRes.numTimesHit == 1)
    assert(counterRes.rawDamage == charResult.damageDealt * 2, "test1")
    assert(counterRes.damageDealt > 0, "test2")
    assert(venusaur.currentHP() < intermedHP, "test3")
  }

  it should "be unaffected by switching out and then back in" in {
    val f = megaFixture(
      List(("Charizard", 100), ("Magikarp", 100)), List(("Venusaur", 100)),
      List(List("Counter"), List("Splash")), List(List("Tackle")))
    import f._
    assert(team1.activePokemon.name == "charizard")

    val tackleRes = team2.useMove(1, team1, battle)  // tackle Charizard
    assert(team1.activePokemon.currentHP() < team1.activePokemon.maxHP)

    team1.switch(2, battle)
    assert(team1.activePokemon.name == "magikarp")
    team1.useMove(1, team2, battle)  // splash splash
    team1.switch(1, battle)  // charizard back
    assert(team1.activePokemon.name == "charizard")

    val counterRes = team1.useMove(1, team2, battle)
    assert(counterRes.numTimesHit == 1)
    assert(counterRes.rawDamage == tackleRes.damageDealt * 2, "test1")
    assert(counterRes.damageDealt > 0, "test2")
    assert(team1.activePokemon.currentHP() < team1.activePokemon.maxHP, "test3")
  }

  it should "only use the last hit of a DoubleStrike attack" in {
    val f = singleMoveFixture(MoveDepot("counter"))
    import f._
    
    val r = MoveDepot("doublekick").use(venusaur, 5, charizard, battle)  // Fighting
    val counterRes = charizard.useMove(1, venusaur, battle)
    assert(counterRes.numTimesHit == 1)
    assert(counterRes.rawDamage == r.damageDealt * 2)  // damageDealt refers to last strike
    // DoubleStrike on full-health Charizard will hit twice, both same damage
    // Counter will take one strike, double it, and send it back to Venusaur
    // So this cute relationship holds:
    assert(charizard.maxHP - charizard.currentHP() == venusaur.maxHP - venusaur.currentHP())
  }

  it should "only use the last hit of a MultiStrike attack" in {
    val f = singleMoveFixture(MoveDepot("counter"))
    import f._
    
    // same damage each strike, 100% accuracy
    val r = (new TestPhysicalMultiStrike).use(venusaur, 5, charizard, battle)
    val t = totalDamageDealt(r)
    val counterRes = charizard.useMove(1, venusaur, battle)
    assert(counterRes.damageDealt == 2 * r.damageDealt)
    if (r.numTimesHit > 2)
      assert(counterRes.damageDealt < t)
  }

  it should "counter the amount of damage the underlying Pokemon would have taken if damage wasn't absorbed by a sub" in {
    // We need a Pokemon that knows both Counter AND Substitute
    val f = fullFixture(100, 100, 
      List(MoveDepot("counter"), MoveDepot("substitute")), List(MoveDepot("tackle")))
    import f._

    reduceHPTo(p1, 99)
    p1.useMove(2, p2, battle) // charizard makes a sub
    assert(p1.currentHP(false) == 90) // sub has 90 HP
    assert(p1.currentHP(true) == 10)  // charizard has 10 HP

    val r = p2.useMove(1, p1, battle)  // tackle charizard
    val counterRes = p1.useMove(1, p2, battle)  // Counter tackle
    assert(counterRes.numTimesHit == 1)
    assert(counterRes.damageDealt == 20)
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

object CounterSpec extends Properties("Counter") {
  import Prop.{forAll, BooleanOperators}

  property("Counter should deal 2x damage back against Normal damaging move") = {
    forAll(randomMove) {
      m => {
        (m.type1 == Normal) ==> {
          val opponentWhoCanLearnM: Int = LearnsetData.getPokemonIndexWhoCanLearn(m.index)
          val f = fullFixture(
            100, 100,
            List(MoveDepot("counter")), List(m),
            "Charizard", PokeData.idToName(opponentWhoCanLearnM))
          import f._

          val enemyResult = p2.useMove(1, p1, battle)
          (battle.counterMan.lastDamageDealtMR(p1).isDefined)
          if (p1.isAlive && p2.isAlive) {  // it was failing on Explosion, since p2 was dead
            val result = p1.useMove(1, p2, battle) // Counter the Normal move
            if (enemyResult.damageDealt > 0)
              (result.damageDealt == enemyResult.damageDealt * 2)
            else
              (result.damageDealt == 0)
          } else (1 == 1)
        }
      }
    }
  }

  // property("Counter should deal 2x damage back against Fighting damaging move") = {
  //   forAll(randomMove) {
  //     m => {
  //       (m.type1 == Fighting) ==> {
  //         val opponentWhoCanLearnM: Int = LearnsetData.getPokemonIndexWhoCanLearn(m.index)
  //         val f = fullFixture(
  //           100, 100,
  //           List(MoveDepot("counter")), List(m),
  //           "Charizard", PokeData.idToName(opponentWhoCanLearnM))
  //         import f._

  //         val enemyResult = p2.useMove(1, p1, battle)
  //         (battle.counterMan.lastDamageDealtMR(p1).isDefined)
  //         if (p1.isAlive && p2.isAlive) {  // it was failing on Explosion, since p2 was dead
  //           val result = p1.useMove(1, p2, battle) // Counter the Fighting move
  //           if (enemyResult.damageDealt > 0)
  //             (result.damageDealt == enemyResult.damageDealt * 2)
  //           else
  //             (result.damageDealt == 0)
  //         } else (1 == 1)
  //       }
  //     }
  //   }
  // }

}
