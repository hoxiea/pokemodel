package pokemodel

import scala.collection.mutable
import Type._
import StatusAilment._
import CritHitType._
import scala.util.Random

// TODO: Add the fact that only centain Pokemon can learn certain moves
// Can be scraped from pages such as
// http://bulbapedia.bulbagarden.net/wiki/Charmander_(Pok%C3%A9mon)/Generation_I_learnset

// TODO: There are really just two critHitRates: normal, and high. Make a little Enum for them
// TODO: I think that traits are the way to go for at least some of the moves

/* In the game, each Move is stored exactly once in memory, and the Pokemon Data Structure keeps
 * track of which Moves the Pokemon knows and how many PP are left for each Move the Pokemon has.
 * 
 * I modeled Moves as objects that maintain their own statistics and state, and know how to do things
 * like use themselves against another Pokemon.
 * 
 * I originally made the constructor value a Pokemon, but this became a Builder/Pokemon issue:
 * if a Move really needs a Pokemon to be created, then you can't really load a Move into a PokemonBuilder,
 * since the Builder hasn't built the Pokemon yet.
 *  
 * With the constructor value an Option[Pokemon], the Move can start without a Pokemon (which kind of makes sense
 * to think about anyway), and then when a Pokemon is created from a Builder, it can pass itself as the Move owner.
 */

abstract class Move {  
  /* ABSTRACT STUFF */
  val index : Int                // in 1 .. 165
  val accuracy : Double          // in [0.0, 1.0]
  val type1 : Type.Value
  val power : Int
  val priority : Int
  val maxPP : Int
  var currentPP : Int
  def use(attacker: Pokemon, defender: Pokemon, pb: Battle)

  /* IMPEMENTED STUFF */
  val critHitRate = LOW   // True for 99% of moves, outliers can override
  def restorePP(amount : Int) = { currentPP = maxPP min (currentPP + amount) }
  def restorePP() = { currentPP = maxPP }
    
  override def toString() = {
    val moveName = this.getClass().getName()
    val prefix = "pokemodel."
    if (moveName.startsWith(prefix)) moveName.substring(prefix.length)
    else moveName
  }
}

/* PHYSICAL MOVES */
abstract class PhysicalMove extends Move {
  def getAttackStat(attacker: Pokemon, b : Battle)  = b.statManager.getEffectiveAttack(attacker)
  def getDefenseStat(defender: Pokemon, b : Battle) = b.statManager.getEffectiveDefense(defender)
}

class Pound extends PhysicalMove {
  val index = 1
  val accuracy = 1.0          // in [0.0, 1.0]
  val type1 = Normal
  val power = 40
  val priority = 0
  val maxPP = 35
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    val chanceHit = accuracy * (pb.statManager.getEffectiveAccuracy(attacker).toDouble / pb.statManager.getEffectiveEvasion(defender))
    if (Random.nextDouble < chanceHit) {
      val damageDealt = pb.dc.calc(attacker,
    		  					   defender,
                                   getAttackStat(attacker, pb),
                                   getDefenseStat(attacker, pb),
                                   this)
      defender.takeDamage(damageDealt)
    }
  }
}


/* SPECIAL MOVES */
abstract class SpecialMove extends Move {
  def getAttackStat(attacker: Pokemon) = attacker.special
  def getDefenseStat(defender: Pokemon) = defender.special
}

class DragonRage extends SpecialMove {
  val index = 82
  val accuracy = 1.0          // in [0.0, 1.0]
  val type1 = Dragon
  val power = 0
  val priority = 0
  val maxPP = 10
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = { 
    defender.takeDamage(40)
    currentPP -= 1
  }
}

class SonicBoom extends SpecialMove {
  val index = 49
  val accuracy = 0.9          // in [0.0, 1.0]
  val type1 = Normal
  val power = 0
  val priority = 0
  val maxPP = 20
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextFloat < accuracy) { defender.takeDamage(20) }
    currentPP -= 1
  }
}

class Thunder extends SpecialMove {
  val index = 87
  val accuracy = 0.7          // in [0.0, 1.0]
  val type1 = Electric
  val power = 110
  val priority = 0
  val maxPP = 10
  var currentPP = maxPP
  
  val parChance = 0.1

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // TODO: use Pound as a reference point
    currentPP -= 1
  }
}


/* STATUS MOVES */
abstract class StatusMove extends Move

class Struggle extends PhysicalMove {
  val index = 165
  val accuracy = 0.0
  val type1 = Normal
  val power = 50
  val priority = 0
  val maxPP = 1
  var currentPP = 1

  override def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // Attack!
    
    // Take 1/2 damage dealt as recoil
  }
}
