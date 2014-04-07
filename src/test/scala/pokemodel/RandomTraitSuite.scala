package pokemodel 

import org.scalacheck._
import Prop.{forAll, BooleanOperators}

object RandomTestingStuff {
  val pokemonLevel = Gen.choose(5,100)   // inclusive on both ends
  val pokemonIndex = Gen.choose(5,100)   // inclusive on both ends

  def fixture(
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

  def reduceHPTo(p: Pokemon, newHP: Int) {
    // Quick way to reduce the HP of Pokemon p down to newHP
    p.takeDamage(p.currentHP() - newHP)
    assert(p.currentHP() == newHP)
  }
}

object DamageEqualsUserLevelSpec extends Properties("DamageEqualsUserLevel") {
  import RandomTestingStuff._

  property("appropriate damage dealt against full-health enemy") = 
    forAll(pokemonLevel) { 
      level => {
        // p2Level = 100 means that opponent's maxHP > 100
        val f = fixture(level, 100, List(new TestDEUL), List(MoveDepot("substitute")))
        import f._

        val result = p1.useMove(1, p2, battle)
        result.damageCalc == level && result.damageDealt == level
      }
  }

  property("correct damageCalc and damageDealt against weakened enemy if appropriate") =
    forAll(pokemonLevel) {
      level => {
        val f = fixture(level, 100, List(new TestDEUL), List(MoveDepot("substitute")))
        import f._
        val enemyHP = 50
        reduceHPTo(p2, enemyHP)

        val result = p1.useMove(1, p2, battle)
        result.damageCalc == level && 
        result.damageDealt == (level min enemyHP)
      }
    }
}

// object GainPropDamageDealt extends Properties("GainPropDamageDealt") {
//   import RandomTestingStuff._

//   property("No effect if you already have full health") = 
//     forAll(pokemonLevel) { 
//       level => {
//         val f = fixture(level, 100, List(new TestDEUL), List(MoveDepot("substitute")))
//         import f._

//         val result = p1.useMove(1, p2, battle)
//         result.damageCalc == level && result.damageDealt == level
//       }
//   }
// }

