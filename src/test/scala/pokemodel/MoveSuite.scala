package pokemodel

import org.scalatest.FunSuite

class MoveSuite extends FunSuite {
  test("Playing with traits - VineWhip") {
    val venusaur = new Pokemon(new PokemonBuilder("Venusaur", 50)
                                   .maxOut().
                                   move(1, new TestPhysicalSingleStrike))
    val machop = new Pokemon(new PokemonBuilder("Machop", 50).maxOut().move(1, new KarateChop))
    val team1 = new PokemonTeam(venusaur)
    val team2 = new PokemonTeam(machop)
    val trainer1 = new UseFirstAvailableMove(team1)
    val trainer2 = new UseFirstAvailableMove(team2)
    val battle = new Battle(trainer1, trainer2)
    println(battle)
    val critHit: Boolean = venusaur.useMove(1, machop, battle)
    val damageDealt = machop.maxHP - machop.currentHP
    if (!critHit) assert(21 <= damageDealt && damageDealt <= 25)  // values from math.miami calculator
    else assert(39 <= damageDealt && damageDealt <= 46)
  }
}
