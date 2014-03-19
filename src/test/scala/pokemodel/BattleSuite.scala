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
  
  test("using Cause40Damage should cause HP to drop by 40 or kill Pokemon") {
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
}