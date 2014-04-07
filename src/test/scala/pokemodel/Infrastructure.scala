package pokemodel

import org.scalacheck._
import Prop.{forAll, BooleanOperators}

object TestingInfrastructure {
  def singleMoveFixture(m: Move) =
    new {
      val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, m)
      val charizard = new Pokemon(pb1)
      val pb2 = new PokemonBuilder("Venusaur", 100).maxOut().move(1, "substitute")
      val venusaur = new Pokemon(pb2)
      val team1 = new PokemonTeam(charizard)
      val team2 = new PokemonTeam(venusaur)
      val trainer1 = new UseFirstAvailableMove(team1)
      val trainer2 = new UseFirstAvailableMove(team2)
      val battle = new Battle(trainer1, trainer2)
      // don't actually attack, so that you can make mutations first
    }

  def subFixture(m: Move) =
    // Send Charizard up against a Venusaur with 10HP and a sub with 91 HP
    new {
      val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, m)
      val charizard = new Pokemon(pb1)
      val pb2 = new PokemonBuilder("Venusaur", 100).maxOut().move(1, "substitute")
      val venusaur = new Pokemon(pb2)
      val team1 = new PokemonTeam(charizard)
      val team2 = new PokemonTeam(venusaur)
      val trainer1 = new UseFirstAvailableMove(team1)
      val trainer2 = new UseFirstAvailableMove(team2)
      val battle = new Battle(trainer1, trainer2)
      
      // get the sub ready to go
      // 363/4 == 90 == cost of making a substitute
      // So get it to 100 HP, then make sub => sub has 91 HP, venusaur has 10 HP
      val maxHP = 363
      val newHP = 100
      assert(venusaur.maxHP == maxHP)  
      reduceHPTo(venusaur, newHP)
      venusaur.makeSub()
      assert(venusaur.hasSub)
      assert(venusaur.currentHP(false) == 91)    // (maxHP / 4) + 1
      assert(venusaur.currentHP(true) == 10)     // newHP - (maxHP / 4)
    }

  def fullFixture(
    p1Level: Int,
    p2Level: Int,
    p1Moves: List[Move],
    p2Moves: List[Move],
    p1Name: String = "Charizard",
    p2Name: String = "Venusaur") =
    new {
      val pb1 = new PokemonBuilder(p1Name, p1Level).maxOut().learnMoves(p1Moves)
      val p1 = new Pokemon(pb1)
      val pb2 = new PokemonBuilder(p2Name, p2Level).maxOut().learnMoves(p2Moves)
      val p2 = new Pokemon(pb2)
      val team1 = new PokemonTeam(p1)
      val team2 = new PokemonTeam(p2)
      val trainer1 = new UseFirstAvailableMove(team1)
      val trainer2 = new UseFirstAvailableMove(team2)
      val battle = new Battle(trainer1, trainer2)
      // don't actually attack, so that you can make mutations first
    }

  /* HELPER FUNCTIONS */
  def totalDamageDealt(msResult: MoveResult) =
    // For a MultiStrike move, damageCalc is the value returned by the
    // DamageCalculator, which would ideally be how much damage is dealt
    // numTimesHit times. But MultiStrike moves are designed to stop if they
    // break a sub or KO the opponent. This function tells you how much damage
    // the MultiStrike move whose result is msResult dealt total.
    (msResult.numTimesHit - 1) * msResult.damageCalc + msResult.damageDealt

  def reduceHPTo(p: Pokemon, newHP: Int) {
    // Quick way to reduce the HP of Pokemon p down to newHP
    require(newHP <= p.maxHP)
    p.takeDamage(p.currentHP() - newHP)
    assert(p.currentHP() == newHP)
  }

  /* STUFF FOR RANDOM TESTING VIA SCALACHECK */
  val pokemonLevel = Gen.choose(5,100)   // inclusive on both ends
  val pokemonIndex = Gen.choose(1,150)   // inclusive on both ends
}
