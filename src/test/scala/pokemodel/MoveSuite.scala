package pokemodel

import org.scalatest.FunSuite

class MoveSuite extends FunSuite {
  test("Playing with traits - VineWhip") {
    val venusaur = new Pokemon(new PokemonBuilder("Venusaur", 50).maxOut().move(1, new TestPhysicalSingleStrike))
    val machop = new Pokemon(new PokemonBuilder("Machop", 50).maxOut().move(1, new KarateChop))   // NoMove is default
    val team1 = new PokemonTeam(venusaur)
    val team2 = new PokemonTeam(machop)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
    venusaur.useMove(1, machop, battle)
  }
}