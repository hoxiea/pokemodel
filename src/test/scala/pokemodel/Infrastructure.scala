package pokemodel

import org.scalacheck._
import Prop.{forAll, BooleanOperators}

object TestingInfrastructure {
  trait Accuracy80 extends Move {override val accuracy = 0.80}
  trait Accuracy50 extends Move {override val accuracy = 0.50}
  trait Accuracy30 extends Move {override val accuracy = 0.30}
  trait Accuracy10 extends Move {override val accuracy = 0.10}

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
      val pb2 = new PokemonBuilder("Venusaur", 100).maxOut()
                    .move(1, "substitute")
                    .move(2, "tackle")
                    .move(3, "vinewhip")
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

  // These two helpers are used in megaFixture
  private def movenamesToMoves(movenames: List[String]): List[Move] =
    movenames.map(name => MoveDepot(name))

  private def processZip(combined: ((String, Int), List[Move])): Pokemon = {
    val (pokemonName, pokemonLevel) = combined._1
    val pb = new PokemonBuilder(pokemonName, pokemonLevel).maxOut()
    pb.learnMoves(combined._2)
    new Pokemon(pb)
  }

  def megaFixture(
    p1Info: List[(String, Int)],  // (pokemonName, level)
    p2Info: List[(String, Int)],  // (pokemonName, level)
    p1Movenames: List[List[String]],     // move names for each Pokemon in p1s
    p2Movenames: List[List[String]]) =   // move names for each Pokemon in p2s
      new {
        assert(p1Info.length == p1Movenames.length)
        assert(p2Info.length == p2Movenames.length)
        private val p1Moves: List[List[Move]] = p1Movenames.map(movenamesToMoves)
        private val p2Moves: List[List[Move]] = p2Movenames.map(movenamesToMoves)
        private val p1s = p1Info.zip(p1Moves).map(processZip)
        private val p2s = p2Info.zip(p2Moves).map(processZip)

        val team1 = new PokemonTeam(p1s)
        val team2 = new PokemonTeam(p2s)
        val trainer1 = new UseFirstAvailableMove(team1)
        val trainer2 = new UseFirstAvailableMove(team2)
        val battle = new Battle(trainer1, trainer2)
      }


  /* HELPER FUNCTIONS */
  def totalDamageDealt(msResult: MoveResult) = {
    // For a MultiStrike move, rawDamage is the value returned by the
    // DamageCalculator, which would ideally be how much damage is dealt
    // numTimesHit times. But MultiStrike moves are designed to stop if they
    // break a sub or KO the opponent. This function tells you how much damage
    // the MultiStrike move whose result is msResult dealt total.
    require(msResult.numTimesHit >= 1)  // if it stops early, then only 1 strike
    (msResult.numTimesHit - 1) * msResult.rawDamage + msResult.damageDealt
  }

  def reduceHPTo(p: Pokemon, newHP: Int) {
    // Quick way to reduce the HP of Pokemon p down to newHP
    require(newHP <= p.maxHP)
    p.takeDamage(p.currentHP() - newHP)
    assert(p.currentHP() == newHP)
  }

  /* STUFF FOR RANDOM TESTING VIA SCALACHECK */
  val pokemonLevel = Gen.choose(5,100)   // inclusive on both ends
  val pokemonIndex = Gen.choose(1,150)   // inclusive on both ends

  val randomMove = for {
    moveIndex <- Gen.choose(1, 164)  // no Struggle == 165
  } yield MoveDepot(moveIndex)

  val randomPokemon = for {
    i <- Gen.choose(1, 150)  // pokemonIndex
    l <- Gen.choose(1, 100)  // level
  } yield new Pokemon(new PokemonBuilder(i, l).maxOut().addRandomMoves())
}
