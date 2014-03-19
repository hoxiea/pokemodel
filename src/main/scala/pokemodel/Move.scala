package pokemodel

import scala.collection.mutable
import Type._

// TODO: Add the fact that only centain Pokemon can learn certain moves
// Can be scraped from pages such as
// http://bulbapedia.bulbagarden.net/wiki/Charmander_(Pok%C3%A9mon)/Generation_I_learnset

// TODO: There are really just two critHitRates: normal, and high. Make a little Enum for them
// TODO: I think that traits are the way to go for at least some of the moves
abstract class Move (val pokemon : Pokemon) extends Ordered[Move]
{
  /* ABSTRACT STUFF */
  val accuracy : Double          // in [0.0, 1.0]
  val critHitRate : Double       // in [0.0, 1.0]
  val type1 : Type.Value
  val power : Int
  val priority : Int
  val maxPP : Int
  var currentPP : Int
  def use(enemy: Pokemon, pb: Battle)

  /* IMPEMENTED STUFF */
  def restorePP(amount : Int) = { currentPP = maxPP min (currentPP + amount) }
  def restorePP() = { currentPP = maxPP }
  
  def compare(that: Move) = {
    /*
     * Moves can be compared to determine which Move would be executed first on a turn
     * that both Pokemon use moves.
     * move1 < move2 => move1 goes first
     */
    if (priority > that.priority) -1
    else if (priority < that.priority) 1
    // priorities are equal, check Pokemon speeds
    else if (pokemon.speed > that.pokemon.speed) -1
    else if (pokemon.speed > that.pokemon.speed) 1
    else 0
  }
  
  override def toString() = {
    val moveName = this.getClass().getName()
    val prefix = "pokemodel."
    if (moveName.startsWith(prefix)) moveName.substring(prefix.length)
    else moveName
  }
}

class Deal40Damage(override val pokemon: Pokemon) extends Move (pokemon){
  val accuracy = 1.0          // in [0.0, 1.0]
  val critHitRate = 0.0       // in [0.0, 1.0]
  val type1 = Normal
  val power = 40
  val priority = 0
  val maxPP = 20
  var currentPP = maxPP

  def use(enemy: Pokemon, pb: Battle) = { enemy.takeDamage(40) }
}

class Attack(override val pokemon: Pokemon) extends Move (pokemon){
  val accuracy = 1.0          // in [0.0, 1.0]
  val critHitRate = 0.1       // in [0.0, 1.0]
  val type1 = Normal
  val power = 40
  val priority = 0
  val maxPP = 10
  var currentPP = maxPP

  def use(enemy: Pokemon, pb: Battle) = {}
}

class NoMove(override val pokemon : Pokemon) extends Move(pokemon) {
  val accuracy = 0.0
  val critHitRate = 0.0
  val type1 = Normal
  val power = 0
  val priority = 0
  val maxPP = 35
  var currentPP = 0

  override def use(enemy: Pokemon, pb: Battle) = {}
}

class Struggle(override val pokemon : Pokemon) extends Move(pokemon) {
  val accuracy = 0.0
  val critHitRate = 0.0
  val type1 = Normal
  val power = 0
  val priority = 0
  val maxPP = 35
  var currentPP = 0

  override def use(enemy: Pokemon, pb: Battle) = {}
}
