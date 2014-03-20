package pokemodel

import org.scalatest.FunSuite

class BattleSuite extends FunSuite {
  test("using NoMove shouldn't cause anything to happen to opponent") {
    val p1 = new Pokemon(PokemonBuilder.generateRandomPokemonBuilder())   // NoMove is default
    val p2 = new Pokemon(PokemonBuilder.generateRandomPokemonBuilder())   // NoMove is default
    val team1 = new PokemonTeam(p1)
    val team2 = new PokemonTeam(p2)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
    assert(p1.currentHP == p1.maxHP)
    assert(p2.currentHP == p2.maxHP)
  }

  test("Using DragonRage should cause HP to drop by 40 or kill Pokemon") {
    // Make level 99 Pokemon... should have more than 40 HP initially
    val pb1 = PokemonBuilder.generateRandomPokemonBuilder(99).move1(new DragonRage(None))
    val p1 = new Pokemon(pb1)
    val pb2 = PokemonBuilder.generateRandomPokemonBuilder(99).move1(new DragonRage(None))
    val p2 = new Pokemon(pb2)

    val team1 = new PokemonTeam(p1)
    val team2 = new PokemonTeam(p2)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
    battle.nextTurn()
    assert(p1.currentHP == p1.maxHP - 40)
    assert(p2.currentHP == p2.maxHP - 40)
    battle.nextTurn()
  }
  
    test("Using a move should cause PP to decrease by 1") {
    val pb1 = PokemonBuilder.generateRandomPokemonBuilder().move1(new DragonRage(None))
    val p1 = new Pokemon(pb1)
    val pb2 = PokemonBuilder.generateRandomPokemonBuilder().move1(new DragonRage(None))
    val p2 = new Pokemon(pb2)

    val team1 = new PokemonTeam(p1)
    val team2 = new PokemonTeam(p2)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
    battle.nextTurn()
    assert(p1.move1.currentPP == p1.move1.maxPP - 1)
    assert(p2.move1.currentPP == p2.move1.maxPP - 1)
    battle.nextTurn()
  }

}
