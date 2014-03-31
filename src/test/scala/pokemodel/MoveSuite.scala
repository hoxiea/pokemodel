package pokemodel

import org.scalatest.FunSuite

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
    assert (result.STAB == false, "stab")
    assert (result.typeMult == 1.0, "typeMult")
    assert (result.statusChange.isEmpty, "statusChange")
    assert (result.KO == false, "KO")
    assert (result.selfKO == false, "selfKO")
    assert (venusaur.currentHP == venusaur.maxHP - result.damageDealt, "opponent HP")
  }

  test("Trying TestPhysicalSingleStrike, Power80") {
    val m = new TestPhysicalSingleStrike with Power80 with CritHit
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
      assert (58 <= result.damageDealt, "reg damage too low")
      assert (result.damageDealt <= 69, "reg damage too high")
    } else {
      assert (114 <= result.damageDealt, "crithit damage too low")
      assert (result.damageDealt <= 134, "crithit damage too high")
    }
    assert (result.STAB == false, "stab")
    assert (result.typeMult == 1.0, "typeMult")
    assert (result.statusChange.isEmpty, "statusChange")
    assert (result.KO == false, "KO")
    assert (result.selfKO == false, "selfKO")
    assert (venusaur.currentHP == venusaur.maxHP - result.damageDealt, "opponent HP")
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
    assert (result.statusChange.isEmpty, "statusChange")
    assert (result.KO == false, "KO")  // venusaur has 363 HP
    assert (result.selfKO == false, "selfKO")
    assert (venusaur.currentHP == venusaur.maxHP - result.damageDealt)
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
    assert (result.statusChange.isEmpty, "statusChange")
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
    assert (result.statusChange.isEmpty, "statusChange")
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
    assert(p2.currentHP == p2.maxHP - result.damageDealt)
    assert(dragonite.maxHP - dragonite.currentHP == result.damageDealt / 2)
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
    assert(!p2.isAlive, "opponent")
    assert(dragonite.maxHP - dragonite.currentHP == opponentHP / 2, "self")
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
    assert(gengar.currentHP == gengar.maxHP)
    assert(dragonite.currentHP == dragonite.maxHP)

    // Rock - geodude
    val result2 = dragonite.useMove(1, geodude, battle)
    assert(result2.typeMult == 0.5)
    assert(geodude.currentHP == geodude.maxHP - result2.damageDealt)
    assert(dragonite.maxHP - dragonite.currentHP == result2.damageDealt / 2)
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

}
