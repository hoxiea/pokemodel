package pokemodel

import scala.collection.mutable
import Type._
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
  val critHitRate : Double       // in [0.0, 1.0]
  val type1 : Type.Value
  val power : Int
  val priority : Int
  val maxPP : Int
  var currentPP : Int
  def use(attacker: Pokemon, defender: Pokemon, pb: Battle)

  /* IMPEMENTED STUFF */
  def restorePP(amount : Int) = { currentPP = maxPP min (currentPP + amount) }
  def restorePP() = { currentPP = maxPP }
    
  override def toString() = {
    val moveName = this.getClass().getName()
    val prefix = "pokemodel."
    if (moveName.startsWith(prefix)) moveName.substring(prefix.length)
    else moveName
  }
}

abstract class PhysicalMove extends Move {
  def getAttackStat
}

class DragonRage extends Move {
  val index = 82
  val accuracy = 1.0          // in [0.0, 1.0]
  val critHitRate = 0.0       // in [0.0, 1.0]
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

class Pound extends Move {
  // Physical Move - uses Attack and Defense stats
  val index = 1
  val accuracy = 1.0          // in [0.0, 1.0]
  val critHitRate = 0.1       // in [0.0, 1.0]
  val type1 = Normal
  val power = 40
  val priority = 0
  val maxPP = 35
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    println("POUND CALLED!")
    println(s"POUND's Pokemon = attacker")

    val effectiveAccuracy = accuracy      // TODO: take battle accuracy/evasion stages into account
    if (Random.nextDouble < effectiveAccuracy) {
      println("GOING AHEAD WITH POUND!")
    
      // http://bulbapedia.bulbagarden.net/wiki/Damage_modification#Damage_formula
      val baseAttack = attacker.attack
      val baseDefense = defender.defense
      val effectiveAttack = baseAttack    // TODO: take battle stages into account, and BRN, and Reflect + Light Screen, and others
      val effectiveDefense = baseDefense  // TODO: take battle stages into account
      val base = power
      val typeMult = DamageCalculator.calculateTypeMultiplier(this, defender)
      val STAB = if (type1 == attacker.type1 || type1 == attacker.type2) 1.5 else 1.0
      val critical = if (Random.nextFloat() < critHitRate) 2 else 1   // TODO: actually implement critHit logic
      val r = 0.85 + Random.nextDouble * (1.0 - 0.85)  // random between 0.85 and 1.00
    
      val A = (2 * attacker.level.toDouble + 10) / 250
      val B = effectiveAttack.toDouble / effectiveDefense
      val modifier = STAB * typeMult * critical * r
    
      val damage = ((A * B * base + 2) * modifier).toInt
      println(s"damage = $damage")
      defender.takeDamage(damage)
    }      
  }
}
  
class NoMove extends Move {
  val index = -1
  val accuracy = 0.0
  val critHitRate = 0.0
  val type1 = Normal
  val power = 0
  val priority = 0
  val maxPP = 35
  var currentPP = 0

  override def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {}   
}

class Struggle extends Move {
  val index = 165
  val accuracy = 0.0
  val critHitRate = 0.0
  val type1 = Normal
  val power = 0
  val priority = 0
  val maxPP = 35
  var currentPP = 0

  override def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // Attack!
    
    // Take 1/2 damage dealt as recoil
  }
}
