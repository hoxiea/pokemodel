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

  "A SingleStrike move" should "deal the expected amount of damage with Power 40" in {
    val f = singleMoveFixture(new TestPhysicalSingleStrike with Power40)
    import f._
    val result = charizard.useMove(1, venusaur, battle)

    // used calculators for these values
    if (!result.critHit) {
      assert (29 <= result.damageDealt, s"reg damage ${result.damageDealt} too low")
      assert (result.damageDealt <= 35, s"reg damage ${result.damageDealt} too high")
    } else {
      assert (57 <= result.damageDealt, s"crithit damage ${result.damageDealt} too low")
      assert (result.damageDealt <= 68, s"crithit damage ${result.damageDealt} too high")
    }
  }

  it should "deal the expected amount of damage with Power 80" in {
    val f = singleMoveFixture(new TestPhysicalSingleStrike with Power80)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    // used calculator for these values
    if (!result.critHit) {
      assert (58 <= result.damageDealt, s"reg damage ${result.damageDealt} too low (58 min)")
      assert (result.damageDealt <= 69, s"reg damage ${result.damageDealt} too high (69 max)")
    } else {
      // TODO: calculator says 114?
      assert (113 <= result.damageDealt, s"crithit damage ${result.damageDealt} too low (114 min)")
      assert (result.damageDealt <= 134, s"crithit damage ${result.damageDealt} too high (134 max)")
    }
  }

  it should "deal the expected amount of damage with Power 120" in {
    val f = singleMoveFixture(new TestPhysicalSingleStrike with Power120)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    // used calculators for these values
    if (!result.critHit) {
      assert (87 <= result.damageDealt, "reg damage too low")
      assert (result.damageDealt <= 103, "reg damage too high")
    } else {
      assert (170 <= result.damageDealt, "crithit damage too low")
      assert (result.damageDealt <= 200, "crithit damage too high")
    }
  }

  it should "provide correct values to its MoveResultBuilder when it doesn't KO" in {
    val f = singleMoveFixture(new TestPhysicalSingleStrike with Power40)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert (result.numTimesHit == 1, "numTimesHit")
    assert (result.STAB == false, "stab")
    assert (result.typeMult == 1.0, "typeMult")
    assert (result.nvsa.isEmpty, "nvsa")
    assert (result.vsa.isEmpty, "vsa")
    assert (result.KO == false, "KO")
    assert (result.selfKO == false, "selfKO")
    assert (venusaur.currentHP() == venusaur.maxHP - result.damageDealt, "opponent HP")
  }

  it should "provide correct values to its MRB when it does KO via CritHit, STAB, and effectiveness" in {
    val f = fullFixture(100, 60,
        List(new TestPhysicalSingleStrike with Power120 with Fire with AlwaysCritHit), List())
    import f._
    val result = p1.useMove(1, p2, battle)

    // used calculators for these values
    // regular hit: 430 to 506
    // crit hit   : 839 to 986
    // So no matter what happens, Venusaur should die
    assert (result.damageDealt == p2.maxHP, "damageDealt didn't equal Venusaur's maxHP")
    assert (result.STAB == true, "stab")  // Charizard is Type1 Fire
    assert (result.typeMult == 2.0, "typeMult")  // Fire is super effective against Plant
    assert (result.nvsa.isEmpty, "nvsa")
    assert (result.vsa.isEmpty, "vsa")
    assert (result.KO == true, "KO")  // venusaur has 222 HP
    assert (result.selfKO == false, "selfKO")
  }

  "A ConstantDamage Move" should "cause HP to drop by its damageAmount when enemy is full-health" in {
    val f = singleMoveFixture(MoveDepot("dragonrage"))
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert (result.damageDealt == 40, "damageDealt")
    assert (result.STAB == false, "stab")
    assert (result.typeMult == 1.0, "typeMult")
    assert (result.nvsa.isEmpty, "nvsa")
    assert (result.vsa.isEmpty, "vsa")
    assert (result.KO == false, "KO")
    assert (result.selfKO == false, "selfKO")
  }

  it should "should kill opponent if opponent has <=damageAmount HP" in {
    val f = singleMoveFixture(MoveDepot("dragonrage"))
    import f._
    reduceHPTo(venusaur, 30)
    val result = charizard.useMove(1, venusaur, battle)
    assert (result.KO, "KO")
    assert (result.damageCalc == 40)
    assert (result.damageDealt == 30)
  }

  "Struggle" should "deal damage and cause 50% recoil damage" in {
    val f = singleMoveFixture(MoveDepot("struggle"))
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.damageCalc == result.damageDealt)
    assert(venusaur.currentHP() == venusaur.maxHP - result.damageDealt)
    assert(charizard.maxHP - charizard.currentHP() == result.damageDealt / 2)
  }

  it should "only 50% of damage DEALT as recoil, not of damage calculated" in {
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

  // test("Struggle: no PP is deducted when it's used as Move5") {
  //   val pb1 = new PokemonBuilder("Dragonite", 100) // no Moves, will use Move5 Struggle
  //   val dragonite = new Pokemon(pb1)
  //   val pb2 = PokemonBuilder.generateRandomPokemonBuilder(100)
  //   val p2 = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(dragonite)
  //   val team2 = new PokemonTeam(p2)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)

  //   val ppBefore = dragonite.pp5.get
  //   battle.takeNextTurn()
  //   assert(dragonite.pp5.get == ppBefore)
  // }

  // test("Struggle: Normal-type damage, so Rock takes half and Ghost takes none") {
  //   val pb1 = new PokemonBuilder("Dragonite", 100).move(1, "Struggle")
  //   val dragonite = new Pokemon(pb1)
  //   val geodude = new Pokemon(new PokemonBuilder("Geodude", 100))
  //   val gengar = new Pokemon(new PokemonBuilder("Gengar", 100))
  //   val team1 = new PokemonTeam(dragonite)
  //   val team2 = new PokemonTeam(List(geodude, gengar))
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)

  //   // Ghost, do this first so that Dragonite doesn't lose HP
  //   val result1 = dragonite.useMove(1, gengar, battle)
  //   assert(result1.typeMult == 0.0)
  //   assert(gengar.currentHP() == gengar.maxHP)
  //   assert(dragonite.currentHP() == dragonite.maxHP)

  //   // Rock - geodude
  //   val result2 = dragonite.useMove(1, geodude, battle)
  //   assert(result2.typeMult == 0.5)
  //   assert(geodude.currentHP() == geodude.maxHP - result2.damageDealt)
  //   assert(dragonite.maxHP - dragonite.currentHP() == result2.damageDealt / 2)
  // }

  // test("Test StatusChange - BRN") {
  //   val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, new TestBurner)
  //   val charizard = new Pokemon(pb1)
  //   val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
  //   val venusaur = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(charizard)
  //   val team2 = new PokemonTeam(venusaur)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)
  //   val result = charizard.useMove(1, venusaur, battle)
  //   assert(venusaur.isBurned)
  // }

  // test("Test StatusChange - SLP") {
  //   val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, new TestAsleep)
  //   val charizard = new Pokemon(pb1)
  //   val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
  //   val venusaur = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(charizard)
  //   val team2 = new PokemonTeam(venusaur)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)
  //   val result = charizard.useMove(1, venusaur, battle)
  //   assert(venusaur.isAsleep)
  // }

  // test("New statusAilments don't displace older statusAilments") {
  //   val pb1 = new PokemonBuilder("Charizard", 100).maxOut()
  //                                .move(1, new TestBurner)
  //                                .move(2, new TestAsleep)
  //   val charizard = new Pokemon(pb1)
  //   val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
  //   val venusaur = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(charizard)
  //   val team2 = new PokemonTeam(venusaur)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)
  //   charizard.useMove(1, venusaur, battle)
  //   assert(venusaur.isBurned)
  //   charizard.useMove(2, venusaur, battle)
  //   assert(venusaur.isBurned)  // still burned, not asleep
  // }

  // test("Basic Multistrike attack, defender doesn't die") {
  //   val pb1 = new PokemonBuilder("Charizard", 100).maxOut()
  //                                .move(1, new TestPhysicalMultiStrike)
  //   val charizard = new Pokemon(pb1)
  //   val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
  //   val venusaur = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(charizard)
  //   val team2 = new PokemonTeam(venusaur)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)
  //   val result = charizard.useMove(1, venusaur, battle)
  //   assert(venusaur.maxHP - venusaur.currentHP() == result.numTimesHit * result.damageDealt)
  // }

  // test("Basic Multistrike attack, defender dies on first strike") {
  //   val defenderHP = 10
  //   val pb1 = new PokemonBuilder("Charizard", 100).maxOut()
  //                                .move(1, new TestPhysicalMultiStrike)
  //   val charizard = new Pokemon(pb1)
  //   val pb2 = new PokemonBuilder("Venusaur", 100).maxOut().currentHP(defenderHP)
  //   val venusaur = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(charizard)
  //   val team2 = new PokemonTeam(venusaur)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)
  //   val result = charizard.useMove(1, venusaur, battle)
  //   assert(result.numTimesHit == 1)
  //   assert(result.damageDealt == defenderHP)
  //   assert(!venusaur.isAlive)
  // }

  // test("Multistrike attack with STAB and type effectiveness") {
  //   val pb1 = new PokemonBuilder("Charizard", 100).maxOut()
  //                                .move(1, new TestPhysicalMultiStrike with Fire)  //
  //   val charizard = new Pokemon(pb1)
  //   val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
  //   val venusaur = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(charizard)
  //   val team2 = new PokemonTeam(venusaur)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)
  //   val result = charizard.useMove(1, venusaur, battle)
  //   assert(result.STAB)
  //   assert(result.moveType == Fire)
  //   assert(result.typeMult == 2.0)
  // }


  // test("SelfStatChange: increase damage done by Physical Attack via Attack stat increase") {
  //   val pb1 = new PokemonBuilder("Charizard", 100).maxOut()
  //                                .move(1, new TestIncreaseSelfAttackStat)
  //                                .move(2, new TestPhysicalSingleStrike)
  //   val charizard = new Pokemon(pb1)
  //   val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
  //   val venusaur = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(charizard)
  //   val team2 = new PokemonTeam(venusaur)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)

  //   val result1 = charizard.useMove(2, venusaur, battle)  // hit
  //   val result2 = charizard.useMove(1, venusaur, battle)  // attackstage +3
  //   val result3 = charizard.useMove(2, venusaur, battle)  // hit again
  //   assert(result1.damageDealt < result3.damageDealt)
  // }

  // test("CritHit Traits: AlwaysCritHit should always land a crithit") {
  //   val pb1 = new PokemonBuilder("Charizard", 100).maxOut()
  //                                .move(1, new TestPhysicalSingleStrike with AlwaysCritHit)
  //   val charizard = new Pokemon(pb1)
  //   val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
  //   val venusaur = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(charizard)
  //   val team2 = new PokemonTeam(venusaur)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)

  //   val result = charizard.useMove(1, venusaur, battle)
  //   assert(result.critHit)
  // }

  // test("CritHit Traits: NeverCritHit should never land a crithit") {
  //   val pb1 = new PokemonBuilder("Charizard", 100).maxOut()
  //                                .move(1, new TestPhysicalSingleStrike with NeverCritHit)
  //   val charizard = new Pokemon(pb1)
  //   val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
  //   val venusaur = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(charizard)
  //   val team2 = new PokemonTeam(venusaur)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)

  //   val result = charizard.useMove(1, venusaur, battle)
  //   assert(!result.critHit)
  // }

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


  // test("SelfStatChange: decrease damage taken by Physical Attack via Defense stat increase") {
  //   // Critical Hits ignore stat mods, so let's turn them off
  //   val m = new TestPhysicalSingleStrike with NeverCritHit
  //   val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, m)
  //   val charizard = new Pokemon(pb1)
  //   val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
  //                                .move(1, new TestIncreaseSelfDefenseStat)
  //   val venusaur = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(charizard)
  //   val team2 = new PokemonTeam(venusaur)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)

  //   val result1 = charizard.useMove(1, venusaur, battle)  // hit
  //   val result2 = venusaur.useMove(1, charizard, battle)  // defensestage +3
  //   val result3 = charizard.useMove(1, venusaur, battle)  // hit again
  //   assert(result1.damageDealt > result3.damageDealt)
  // }

  // test("EnemyStatChange: deal more damage by decreasing enemy's defense") {
  //   // Critical Hits ignore stat mods, so let's turn them off
  //   val m1 = new TestPhysicalSingleStrike with NeverCritHit
  //   val m2 = new TestDecreaseEnemyDefense
  //   val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, m1).move(2, m2)
  //   val charizard = new Pokemon(pb1)
  //   val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
  //   val venusaur = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(charizard)
  //   val team2 = new PokemonTeam(venusaur)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)

  //   val result1 = charizard.useMove(1, venusaur, battle)  // hit
  //   assert(battle.statManager.getEffectiveDefense(venusaur, battle) == venusaur.defense, "first def")
  //   val result2 = charizard.useMove(2, venusaur, battle)  // lower defense
  //   assert(battle.statManager.getEffectiveDefense(venusaur, battle) < venusaur.defense, "second def")
  //   val result3 = charizard.useMove(1, venusaur, battle)  // hit again
  //   assert(result1.damageDealt < result3.damageDealt, "damage off")
  // }

  // test("EnemyStatChange: take less damage by decreasing enemy's attack") {
  //   // Critical Hits ignore stat mods, so let's turn them off
  //   val m1 = new TestPhysicalSingleStrike with NeverCritHit
  //   val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, m1)
  //   val charizard = new Pokemon(pb1)

  //   val m2 = new TestDecreaseEnemyAttack
  //   val pb2 = new PokemonBuilder("Venusaur", 100).maxOut().move(1, m2)
  //   val venusaur = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(charizard)
  //   val team2 = new PokemonTeam(venusaur)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)

  //   val result1 = charizard.useMove(1, venusaur, battle)  // hit
  //   assert(battle.statManager.getEffectiveAttack(charizard) == charizard.attack)
  //   val result2 = venusaur.useMove(1, charizard, battle)  // lower attack
  //   assert(battle.statManager.getEffectiveAttack(charizard) < charizard.attack)
  //   val result3 = charizard.useMove(1, venusaur, battle)  // hit again
  //   assert(result1.damageDealt > result3.damageDealt)
  // }

  // test("OneHitKO: kill enemy?") {
  //   val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, new TestOneHitKO)
  //   val charizard = new Pokemon(pb1)
  //   val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
  //   val venusaur = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(charizard)
  //   val team2 = new PokemonTeam(venusaur)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)

  //   val result = charizard.useMove(1, venusaur, battle)  // hit
  //   assert(result.damageDealt == venusaur.maxHP, "damageDealt error")
  //   assert(result.numTimesHit == 1)
  //   assert(result.KO)
  //   assert(!result.selfKO)
  // }

  // test("OneHitKO: break substitute?") {
  //   val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, new TestOneHitKO)
  //   val charizard = new Pokemon(pb1)
  //   val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
  //   val venusaur = new Pokemon(pb2)
  //   val team1 = new PokemonTeam(charizard)
  //   val team2 = new PokemonTeam(venusaur)
  //   val trainer1 = new UseFirstAvailableMove(team1)
  //   val trainer2 = new UseFirstAvailableMove(team2)
  //   val battle = new Battle(trainer1, trainer2)

  //   assert(venusaur.canMakeSub)
  //   venusaur.makeSub()
  //   val result = charizard.useMove(1, venusaur, battle)  // hit
  //   assert(result.numTimesHit == 1)
  //   assert(!result.KO)
  //   assert(!result.selfKO)
  //   assert(result.subKO)
  // }

  // TODO: Test every Move trait, using the descriptions of the move's behavior as spec instead of code
  // TODO: Test every combination of Move trait that appears in ActualMoves
  // TODO: Test every hand-rolled Move moveSpecificStuff, using move's behavior as spec instead of code
  // TODO: Test SingleStrike requiredStatusAilments, for example that DreamEater only succeeds when the opponent is asleep
}
