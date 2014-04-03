package pokemodel

import org.scalatest.FunSuite

import Type._

class MoveSuite extends FunSuite {

  test("Using a move should cause PP to decrease by 1") {
    val pb1 = new PokemonBuilder("Dragonite", 100).move(1, new DragonRage)
    val dragonite = new Pokemon(pb1)
    val pb2 = PokemonBuilder.generateRandomPokemonBuilder(99)
    val p2 = new Pokemon(pb2)
    val team1 = new PokemonTeam(dragonite)
    val team2 = new PokemonTeam(p2)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
    dragonite.useMove(1, p2, battle)
    assert(dragonite.getPP(1).get == dragonite.getMove(1).get.maxPP - 1)
  }

  test("Trying TestPhysicalSingleStrike, Power40") {
    val pb1 = new PokemonBuilder("Charizard", 100)
                  .move(1, new TestPhysicalSingleStrike with Power40)
                  .maxOut()
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
    val result = charizard.useMove(1, venusaur, battle)

    // used calculators for these values
    if (!result.critHit) {
      assert (29 <= result.damageDealt, s"reg damage ${result.damageDealt} too low")
      assert (result.damageDealt <= 35, s"reg damage ${result.damageDealt} too high")
    } else {
      assert (57 <= result.damageDealt, s"crithit damage ${result.damageDealt} too low")
      assert (result.damageDealt <= 68, s"crithit damage ${result.damageDealt} too high")
    }
    assert (result.numTimesHit == 1, "numTimesHit")
    assert (result.STAB == false, "stab")
    assert (result.typeMult == 1.0, "typeMult")
    assert (result.nvsa.isEmpty, "nvsa")
    assert (result.vsa.isEmpty, "vsa")
    assert (result.KO == false, "KO")
    assert (result.selfKO == false, "selfKO")
    assert (venusaur.currentHP() == venusaur.maxHP - result.damageDealt, "opponent HP")
  }

  test("Trying TestPhysicalSingleStrike, Power80") {
    val m = new TestPhysicalSingleStrike with Power80 with HighCritHit
    val pb1 = new PokemonBuilder("Charizard", 100)
                  .move(1, m)
                  .maxOut()
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
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
    assert (result.STAB == false, "stab")
    assert (result.typeMult == 1.0, "typeMult")
    assert (result.nvsa.isEmpty, "nvsa")
    assert (result.vsa.isEmpty, "vsa")
    assert (result.KO == false, "KO")
    assert (result.selfKO == false, "selfKO")
    assert (venusaur.currentHP() == venusaur.maxHP - result.damageDealt, "opponent HP")
  }

  test("Trying TestPhysicalSingleStrike, Power120") {
    val pb1 = new PokemonBuilder("Charizard", 100)
                  .move(1, new TestPhysicalSingleStrike with Power120)
                  .maxOut()
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
    val result = charizard.useMove(1, venusaur, battle)

    // used calculators for these values
    if (!result.critHit) {
      assert (87 <= result.damageDealt, "reg damage too low")
      assert (result.damageDealt <= 103, "reg damage too high")
    } else {
      assert (170 <= result.damageDealt, "crithit damage too low")
      assert (result.damageDealt <= 200, "crithit damage too high")
    }
    assert (result.STAB == false, "stab")
    assert (result.typeMult == 1.0, "typeMult")
    assert (result.nvsa.isEmpty, "nvsa")
    assert (result.vsa.isEmpty, "vsa")
    assert (result.KO == false, "KO")  // venusaur has 363 HP
    assert (result.selfKO == false, "selfKO")
    assert (venusaur.currentHP() == venusaur.maxHP - result.damageDealt)
  }

  test("Trying TestPhysicalSingleStrike: Power120 + Fire + Weak Opponent = KO!") {
    val pb1 = new PokemonBuilder("Charizard", 100)
                  .move(1, new TestPhysicalSingleStrike with Power120 with Fire)
                  .maxOut()
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 60).maxOut()    // LOWER LEVEL, ENSURES KO
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
    val result = charizard.useMove(1, venusaur, battle)
    // used calculators for these values
    // regular hit: 430 to 506
    // crit hit   : 839 to 986
    // So no matter what happens, Venusaur should die
    assert (result.damageDealt == venusaur.maxHP, "damageDealt didn't equal Venusaur's maxHP")
    assert (result.STAB == true, "stab")  // Charizard is Type1 Fire
    assert (result.typeMult == 2.0, "typeMult")  // Fire is super effective against Plant
    assert (result.nvsa.isEmpty, "nvsa")
    assert (result.vsa.isEmpty, "vsa")
    assert (result.KO == true, "KO")  // venusaur has 222 HP
    assert (result.selfKO == false, "selfKO")
  }

  test("Using DragonRage should cause HP to drop by 40 if opponent has >40 HP") {
    val pb1 = new PokemonBuilder("Dragonite", 100).move(1, new DragonRage)
    val dragonite = new Pokemon(pb1)
    val pb2 = PokemonBuilder.generateRandomPokemonBuilder(99)  // full health, level 99
    val p2 = new Pokemon(pb2)
    val team1 = new PokemonTeam(dragonite)
    val team2 = new PokemonTeam(p2)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    val result = dragonite.useMove(1, p2, battle)
    assert (result.damageDealt == 40, "damageDealt")
    assert (result.STAB == false, "stab")
    assert (result.typeMult == 1.0, "typeMult")
    assert (result.nvsa.isEmpty, "nvsa")
    assert (result.vsa.isEmpty, "vsa")
    assert (result.KO == false, "KO")
    assert (result.selfKO == false, "selfKO")
  }

  test("Using DragonRage should kill opponent if opponent has <=40 HP") {
    val pb1 = new PokemonBuilder("Dragonite", 100).move(1, new DragonRage)
    val dragonite = new Pokemon(pb1)
    val pb2 = PokemonBuilder.generateRandomPokemonBuilder(99).currentHP(30)
    val p2 = new Pokemon(pb2)
    val team1 = new PokemonTeam(dragonite)
    val team2 = new PokemonTeam(p2)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    val result = dragonite.useMove(1, p2, battle)
    assert (result.KO == true, "KO")
  }

  test("Struggle: test attack damage dealt and recoil received") {
    val pb1 = new PokemonBuilder("Dragonite", 100).move(1, new Struggle)
    val dragonite = new Pokemon(pb1)
    val pb2 = PokemonBuilder.generateRandomPokemonBuilder(100)
    val p2 = new Pokemon(pb2)
    val team1 = new PokemonTeam(dragonite)
    val team2 = new PokemonTeam(p2)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    val result = dragonite.useMove(1, p2, battle)
    assert(p2.currentHP() == p2.maxHP - result.damageDealt)
    assert(dragonite.maxHP - dragonite.currentHP() == result.damageDealt / 2)
  }

  test("Struggle: only receive 50% of damage DEALT, not damage calculated") {
    // DamageCalculator will tell us that a level 100 Dragonite should deal a
    // lot more than 10 damage using Struggle, but the recoil on Struggle is
    // 50% of the damage actually dealt, not the potential damage. So give the
    // opponent 11 HP and make sure we only lose 5 using Struggle (checks round-down)
    val opponentHP = 11
    val pb1 = new PokemonBuilder("Dragonite", 100).move(1, new Struggle)
    val dragonite = new Pokemon(pb1)
    val pb2 = PokemonBuilder.generateRandomPokemonBuilder(100).currentHP(opponentHP)
    val p2 = new Pokemon(pb2)
    val team1 = new PokemonTeam(dragonite)
    val team2 = new PokemonTeam(p2)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    val result = dragonite.useMove(1, p2, battle)
    if (result.damageDealt > 0) {
      assert(!p2.isAlive, "opponent")
      assert(dragonite.maxHP - dragonite.currentHP() == opponentHP / 2, "self")
    }
  }

  test("Struggle: no PP is deducted when it's used as Move5") {
    val pb1 = new PokemonBuilder("Dragonite", 100) // no Moves, will use Move5 Struggle
    val dragonite = new Pokemon(pb1)
    val pb2 = PokemonBuilder.generateRandomPokemonBuilder(100)
    val p2 = new Pokemon(pb2)
    val team1 = new PokemonTeam(dragonite)
    val team2 = new PokemonTeam(p2)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    val ppBefore = dragonite.pp5.get
    battle.takeNextTurn()
    assert(dragonite.pp5.get == ppBefore)
  }

  test("Struggle: Normal-type damage, so Rock takes half and Ghost takes none") {
    val pb1 = new PokemonBuilder("Dragonite", 100).move(1, new Struggle)
    val dragonite = new Pokemon(pb1)
    val geodude = new Pokemon(new PokemonBuilder("Geodude", 100))
    val gengar = new Pokemon(new PokemonBuilder("Gengar", 100))
    val team1 = new PokemonTeam(dragonite)
    val team2 = new PokemonTeam(List(geodude, gengar))
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    // Ghost, do this first so that Dragonite doesn't lose HP
    val result1 = dragonite.useMove(1, gengar, battle)
    assert(result1.typeMult == 0.0)
    assert(gengar.currentHP() == gengar.maxHP)
    assert(dragonite.currentHP() == dragonite.maxHP)

    // Rock - geodude
    val result2 = dragonite.useMove(1, geodude, battle)
    assert(result2.typeMult == 0.5)
    assert(geodude.currentHP() == geodude.maxHP - result2.damageDealt)
    assert(dragonite.maxHP - dragonite.currentHP() == result2.damageDealt / 2)
  }

  test("Test StatusChange - BRN") {
    val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, new TestBurner)
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
    val result = charizard.useMove(1, venusaur, battle)
    assert(venusaur.isBurned)
  }

  test("Test StatusChange - SLP") {
    val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, new TestAsleep)
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
    val result = charizard.useMove(1, venusaur, battle)
    assert(venusaur.isAsleep)
  }

  test("New statusAilments don't displace older statusAilments") {
    val pb1 = new PokemonBuilder("Charizard", 100).maxOut()
                                 .move(1, new TestBurner)
                                 .move(2, new TestAsleep)
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
    charizard.useMove(1, venusaur, battle)
    assert(venusaur.isBurned)
    charizard.useMove(2, venusaur, battle)
    assert(venusaur.isBurned)  // still burned, not asleep
  }

  test("Basic Multistrike attack, defender doesn't die") {
    val pb1 = new PokemonBuilder("Charizard", 100).maxOut()
                                 .move(1, new TestMultiStrike)
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
    val result = charizard.useMove(1, venusaur, battle)
    assert(venusaur.maxHP - venusaur.currentHP() == result.numTimesHit * result.damageDealt)
  }

  test("Basic Multistrike attack, defender dies on first strike") {
    val defenderHP = 10
    val pb1 = new PokemonBuilder("Charizard", 100).maxOut()
                                 .move(1, new TestMultiStrike)
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut().currentHP(defenderHP)
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.numTimesHit == 1)
    assert(result.damageDealt == defenderHP)
    assert(!venusaur.isAlive)
  }

  test("Multistrike attack with STAB and type effectiveness") {
    val pb1 = new PokemonBuilder("Charizard", 100).maxOut()
                                 .move(1, new TestMultiStrike with Fire)  //
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.STAB)
    assert(result.moveType == Fire)
    assert(result.typeMult == 2.0)
  }


  test("SelfStatChange: increase damage done by Physical Attack via Attack stat increase") {
    val pb1 = new PokemonBuilder("Charizard", 100).maxOut()
                                 .move(1, new TestIncreaseSelfAttackStat)
                                 .move(2, new TestPhysicalSingleStrike)
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    val result1 = charizard.useMove(2, venusaur, battle)  // hit
    val result2 = charizard.useMove(1, venusaur, battle)  // attackstage +3
    val result3 = charizard.useMove(2, venusaur, battle)  // hit again
    assert(result1.damageDealt < result3.damageDealt)
  }

  test("CritHit Traits: AlwaysCritHit should always land a crithit") {
    val pb1 = new PokemonBuilder("Charizard", 100).maxOut()
                                 .move(1, new TestPhysicalSingleStrike with AlwaysCritHit)
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    val result = charizard.useMove(1, venusaur, battle)
    assert(result.critHit)
  }

  test("CritHit Traits: NeverCritHit should never land a crithit") {
    val pb1 = new PokemonBuilder("Charizard", 100).maxOut()
                                 .move(1, new TestPhysicalSingleStrike with NeverCritHit)
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    val result = charizard.useMove(1, venusaur, battle)
    assert(!result.critHit)
  }

  test("Using an attack twice should usually cause different damage values") {
    /*
     * This test revealed something very interesting.
     * I was seeing two cases:
     * 1. The first attack randomly does more damage than the second.
     * The output looked like:
     * critHit damage = 66   // print statement in DamageCalc.calcCriticalHitDamage
     * critHit damage = 61
     * These shouldn't be the same...
     * damageDealt = 66
     * ... other result1 stuff...
     * damageDealt = 66     // WRONG
     * ... other result2 stuff
     * The second value always equaled the first one.
     * But Venusaur had the correct amount of health! maxHP - (66 + 61)
     *
     * 2. The first attack randomly does less damage than the second.
     * The output looked like:
     * critHit damage = 62   // print statement in DamageCalc.calcCriticalHitDamage
     * critHit damage = 66
     * These shouldn't be the same...
     * damageDealt = 62
     * ... other result1 stuff...
     * damageDealt = 66   // truncated, KO occurs
     * KO = true
     * ... other result2 stuff
     * This seems correct. Venusaur also has the correct amount of health
     *
     * Long story short, when you merge two MoveResultBuilders, it takes the
     * larger damageDealt as the new value, since the default is 0. The only
     * way that the first larger value could displace the second smaller value
     * is if the values are persisting somehow.
     *
     * And that suggested that I was getting burned by the old "mutating a
     * parameter with a default value, and the mutations persist" issue that
     * gets mentioned in Python literature sometimes. I hypothesized that
     * SingleStrike does mrb.merge(result) and not result.merge(mrb)... and
     * sure enough, it did. Not anymore.
     *
     * It's funny too, because I initially wrote mrb.merge to leave both MRBs
     * alone and produce a new MRB from scratch instead of mutating. But then
     * I changed things to mutate. I should have known that changing the input
     * parameters was bad practice, though.
     */

    // println("-------------------")
    val m = new TestPhysicalSingleStrike with Power40 with AlwaysCritHit
    val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, m)
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    val result1 = charizard.useMove(1, venusaur, battle)  // hit
    val result2 = charizard.useMove(1, venusaur, battle)  // hit again
    // println("These shouldn't be the same...")
    // println(result1)
    // println(result2)

    // I left the print statements in place so that my big comment above makes
    // more sense. But if smaller values are failing to overwrite larger ones,
    // we won't always have...
    assert(result1.damageDealt + result2.damageDealt == venusaur.maxHP - venusaur.currentHP())
  }


  test("SelfStatChange: decrease damage taken by Physical Attack via Defense stat increase") {
    // Critical Hits ignore stat mods, so let's turn them off
    val m = new TestPhysicalSingleStrike with NeverCritHit
    val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, m)
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
                                 .move(1, new TestIncreaseSelfDefenseStat)
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    val result1 = charizard.useMove(1, venusaur, battle)  // hit
    val result2 = venusaur.useMove(1, charizard, battle)  // defensestage +3
    val result3 = charizard.useMove(1, venusaur, battle)  // hit again
    assert(result1.damageDealt > result3.damageDealt)
  }

  test("EnemyStatChange: deal more damage by decreasing enemy's defense") {
    // Critical Hits ignore stat mods, so let's turn them off
    val m1 = new TestPhysicalSingleStrike with NeverCritHit
    val m2 = new TestDecreaseEnemyDefense
    val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, m1).move(2, m2)
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    val result1 = charizard.useMove(1, venusaur, battle)  // hit
    assert(battle.statManager.getEffectiveDefense(venusaur) == venusaur.defense, "first def")
    val result2 = charizard.useMove(2, venusaur, battle)  // lower defense
    assert(battle.statManager.getEffectiveDefense(venusaur) < venusaur.defense, "second def")
    val result3 = charizard.useMove(1, venusaur, battle)  // hit again
    assert(result1.damageDealt < result3.damageDealt, "damage off")
  }

  test("EnemyStatChange: take less damage by decreasing enemy's attack") {
    // Critical Hits ignore stat mods, so let's turn them off
    val m1 = new TestPhysicalSingleStrike with NeverCritHit
    val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, m1)
    val charizard = new Pokemon(pb1)

    val m2 = new TestDecreaseEnemyAttack
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut().move(1, m2)
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    val result1 = charizard.useMove(1, venusaur, battle)  // hit
    assert(battle.statManager.getEffectiveAttack(charizard) == charizard.attack)
    val result2 = venusaur.useMove(1, charizard, battle)  // lower attack
    assert(battle.statManager.getEffectiveAttack(charizard) < charizard.attack)
    val result3 = charizard.useMove(1, venusaur, battle)  // hit again
    assert(result1.damageDealt > result3.damageDealt)
  }

  test("OneHitKO: kill enemy?") {
    val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, new TestOneHitKO)
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    val result = charizard.useMove(1, venusaur, battle)  // hit
    assert(result.damageDealt == venusaur.maxHP, "damageDealt error")
    assert(result.numTimesHit == 1)
    assert(result.KO)
    assert(!result.selfKO)
  }

  test("OneHitKO: break substitute?") {
    val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, new TestOneHitKO)
    val charizard = new Pokemon(pb1)
    val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
    val venusaur = new Pokemon(pb2)
    val team1 = new PokemonTeam(charizard)
    val team2 = new PokemonTeam(venusaur)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    assert(venusaur.tryToMakeSub())
    val result = charizard.useMove(1, venusaur, battle)  // hit
    assert(result.numTimesHit == 1)
    assert(!result.KO)
    assert(!result.selfKO)
    assert(result.subKO)
  }

  // TODO: Test every Move trait, using the descriptions of the move's behavior as spec instead of code
  // TODO: Test every hand-rolled Move moveSpecificStuff, using move's behavior as spec instead of code
  // TODO: Test SingleStrike requiredStatusAilments, for example that DreamEater only succeeds when the opponent is asleep
}
