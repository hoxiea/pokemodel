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

  test("Using DragonRage should cause HP to drop by 40 if opponent has >40 HP") {
    val pb1 = new PokemonBuilder(149, 100).move(1, new DragonRage)  // Dragonite, ROAR!
    val p1 = new Pokemon(pb1)
    val pb2 = PokemonBuilder.generateRandomPokemonBuilder(99)  // full health, level 99
    val p2 = new Pokemon(pb2)
    val team1 = new PokemonTeam(p1)
    val team2 = new PokemonTeam(p2)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    // moves will almost never be used like this, but the opponent was using Struggle and hurting himself,
    // so the damages didn't line up
    p1.move1.get.use(p1, p2, battle) 
    assert(p2.currentHP == p2.maxHP - 40)
  }
    
  test("Using a move should cause PP to decrease by 1") {
    val pb1 = new PokemonBuilder(149, 100).move(1, new DragonRage)
    val p1 = new Pokemon(pb1)
    val pb2 = PokemonBuilder.generateRandomPokemonBuilder(99)
    val p2 = new Pokemon(pb2)
    val team1 = new PokemonTeam(p1)
    val team2 = new PokemonTeam(p2)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)

    // moves will almost never be used like this, but the opponent was using Struggle and hurting himself,
    // so the damages didn't line up
    p1.move1.get.use(p1, p2, battle) 
    assert(p1.pp1.get == p1.move1.get.maxPP - 1)
  }
}
