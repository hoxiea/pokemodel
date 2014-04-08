package pokemodel

import org.scalatest._

import Type._
import TestingInfrastructure._

class MoveSuite extends FlatSpec with Matchers {

  "Using a move" should "cause PP to decrease by 1" in {
    val moveName = "slash"
    val f = singleMoveFixture(MoveDepot(moveName))
    import f._
    charizard.useMove(1, venusaur, battle)
    assert(charizard.getPP(1).get == MoveDepot.maxPP(moveName) - 1)
  }

  "Struggle" should "deal damage and cause 50% recoil damage" in {
    val f = singleMoveFixture(MoveDepot("struggle"))
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.damageCalc == result.damageDealt)
    assert(venusaur.currentHP() == venusaur.maxHP - result.damageDealt)
    assert(charizard.maxHP - charizard.currentHP() == result.damageDealt / 2)
  }

  it should "only self-inflict 50% of damage DEALT as recoil, not of damage calculated" in {
    // DamageCalculator will tell us that a level 100 Dragonite should deal a
    // lot more than 10 damage using Struggle, but the recoil on Struggle is
    // 50% of the damage actually dealt, not the potential damage. So give the
    // opponent 11 HP and make sure we only lose 5 using Struggle (checks
    // round-down)
    val f = singleMoveFixture(MoveDepot("struggle"))
    import f._
    val opponentHP = 11
    reduceHPTo(venusaur, opponentHP)

    val result = charizard.useMove(1, venusaur, battle)
    assert(!venusaur.isAlive, "opponent")
    assert(result.KO, "opponent")
    assert(charizard.maxHP - charizard.currentHP() == opponentHP / 2, "self")
  }

  it should "selfKO if you use it with low HP" in {
    val f = singleMoveFixture(MoveDepot("struggle"))
    import f._
    val newHP = 11
    reduceHPTo(charizard, newHP)

    val result = charizard.useMove(1, venusaur, battle)
    // venusaur should survive
    assert(venusaur.isAlive)
    assert(!result.KO)

    // charizard should not
    assert(result.selfKO)
    assert(charizard.currentHP() == 0)
    assert(!charizard.isAlive)
  }

  it should "not have a PP deducted when it's used as Move5" in {
    val f = singleMoveFixture(MoveDepot("struggle"))
    import f._
    val ppBefore = charizard.pp5.get
    battle.takeNextTurn()  // no moves, everyone Struggles
    assert(charizard.currentHP() < charizard.maxHP)
    assert(venusaur.currentHP() < venusaur.maxHP)
    assert(charizard.pp5.get == ppBefore)
  }

  it should "deal Normal-type damage, i.e. half effective against Rock" in {
    val f = fullFixture(100, 100, List(MoveDepot("struggle")), List(), "Charizard", "Geodude")
    import f._

    val result = p1.useMove(1, p2, battle)
    assert(result.typeMult == 0.5)
    assert(p1.maxHP - p1.currentHP() == result.damageDealt / 2)
  }

  it should "deal Normal-type damage, i.e. not effective against Ghost" in {
    val f = fullFixture(100, 100, List(MoveDepot("struggle")), List(), "Charizard", "Gengar")
    import f._

    val result = p1.useMove(1, p2, battle)
    assert(result.typeMult == 0.0)
    assert(p2.maxHP == p2.currentHP())  // p2 didn't lose any health
    assert(p1.maxHP - p1.currentHP() == result.damageDealt / 2)
  }


  // test("Using an attack twice should usually cause different damage values") {
  //   /*
  //    * This test revealed something very interesting.
  //    * I was seeing two cases:
  //    * 1. The first attack randomly does more damage than the second.
  //    * The output looked like:
  //    * critHit damage = 66   // print statement in DamageCalc.calcCriticalHitDamage
  //    * critHit damage = 61
  //    * These shouldn't be the same...
  //    * damageDealt = 66
  //    * ... other result1 stuff...
  //    * damageDealt = 66     // WRONG
  //    * ... other result2 stuff
  //    * The second value always equaled the first one.
  //    * But Venusaur had the correct amount of health! maxHP - (66 + 61)
  //    *
  //    * 2. The first attack randomly does less damage than the second.
  //    * The output looked like:
  //    * critHit damage = 62   // print statement in DamageCalc.calcCriticalHitDamage
  //    * critHit damage = 66
  //    * These shouldn't be the same...
  //    * damageDealt = 62
  //    * ... other result1 stuff...
  //    * damageDealt = 66   // truncated, KO occurs
  //    * KO = true
  //    * ... other result2 stuff
  //    * This seems correct. Venusaur also has the correct amount of health
  //    *
  //    * Long story short, when you merge two MoveResultBuilders, it takes the
  //    * larger damageDealt as the new value, since the default is 0. The only
  //    * way that the first larger value could displace the second smaller value
  //    * is if the values are persisting somehow.
  //    *
  //    * And that suggested that I was getting burned by the old "mutating a
  //    * parameter with a default value, and the mutations persist" issue that
  //    * gets mentioned in Python literature sometimes. I hypothesized that
  //    * SingleStrike does mrb.merge(result) and not result.merge(mrb)... and
  //    * sure enough, it did. Not anymore.
  //    *
  //    * It's funny too, because I initially wrote mrb.merge to leave both MRBs
  //    * alone and produce a new MRB from scratch instead of mutating. But then
  //    * I changed things to mutate. I should have known that changing the input
  //    * parameters was bad practice, though.
  //    */

  //   // println("-------------------")
  //   val m = new TestPhysicalSingleStrike with Power40 with AlwaysCritHit
  //   val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, m)
  //   val charizard = new Pokemon(pb1)
  //   val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
  //   val venusaur = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(charizard)
  //   val team2 = new PokemonTeam(venusaur)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)

  //   val result1 = charizard.useMove(1, venusaur, battle)  // hit
  //   val result2 = charizard.useMove(1, venusaur, battle)  // hit again
  //   // println("These shouldn't be the same...")
  //   // println(result1)
  //   // println(result2)

  //   // I left the print statements in place so that my big comment above makes
  //   // more sense. But if smaller values are failing to overwrite larger ones,
  //   // we won't always have...
  //   assert(result1.damageDealt + result2.damageDealt == venusaur.maxHP - venusaur.currentHP())
  // }


  // TODO: Test every Move trait, using the descriptions of the move's behavior as spec
  // TODO: Test every combination of Move trait that appears in ActualMoves
  // TODO: Test every hand-rolled Move moveSpecificStuff, using move's behavior as spec
}
