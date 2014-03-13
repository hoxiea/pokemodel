package pokemodel

import scala.collection.mutable
import Type._

// TODO: Add the fact that only centain Pokemon can learn certain moves
// Can be scraped from pages such as
// http://bulbapedia.bulbagarden.net/wiki/Charmander_(Pok%C3%A9mon)/Generation_I_learnset

// TODO: There are really just two critHitRates: normal, and high. Make a little Enum for them
// TODO: I think that traits are the way to go for at least some of the moves
abstract class Move (val pokemon : Pokemon)
{
  val accuracy : Double          // in [0.0, 1.0]
  val critHitRate : Double       // in [0.0, 1.0]
  val type1 : Type.Value
  val power : Int
  val priority : Int
  val maxPP : Int
  var currentPP : Int

  def use(enemy: Pokemon, pb: Battle)

  def restorePP(amount : Int) = {
    currentPP = maxPP min (currentPP + amount)
  }

  def restorePP() = {
    currentPP = maxPP
  }
}

class Attack(override val pokemon: Pokemon) extends Move (pokemon){
  val accuracy = 0.9          // in [0.0, 1.0]
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
  var currentPP = maxPP

  override def use(enemy: Pokemon, pb: Battle) = {}
}
