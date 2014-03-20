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

abstract class Move (val pokemon: Option[Pokemon]) extends Ordered[Move] {  
  /* ABSTRACT STUFF */
  val index : Int                // in 1 .. 165
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
    else (this.pokemon, that.pokemon) match {
      case (None, None) => 0
      case (Some(p1), None) => throw new Exception("compared move with a Pokemon to move without a Pokemon")
      case (None, Some(p2)) => throw new Exception("compared move with a Pokemon to move without a Pokemon")
      case (Some(p1), Some(p2)) => {
        if (p1.speed > p2.speed) -1
        else if (p1.speed < p2.speed) 1
      	else 0
      }
    }
  }
  
//  def addPokemon(p: Pokemon) : Unit = {
//    pokemon = Some(p)
//  }
  
  override def toString() = {
    val moveName = this.getClass().getName()
    val prefix = "pokemodel."
    if (moveName.startsWith(prefix)) moveName.substring(prefix.length)
    else moveName
  }
}

abstract class PhysicalMove(override var pokemon: Option[Pokemon]) extends Move(pokemon) {
  def getAttackStat
}

class DragonRage(override val pokemon: Option[Pokemon]) extends Move(pokemon) {
  val index = 82
  val accuracy = 1.0          // in [0.0, 1.0]
  val critHitRate = 0.0       // in [0.0, 1.0]
  val type1 = Dragon
  val power = 0
  val priority = 0
  val maxPP = 10
  var currentPP = maxPP

  def use(enemy: Pokemon, pb: Battle) = { 
    enemy.takeDamage(40)
    currentPP -= 1
  }
}

class Pound(override val pokemon: Option[Pokemon]) extends Move (pokemon) {
  // Physical Move - uses Attack and Defense stats
  val index = 1
  val accuracy = 1.0          // in [0.0, 1.0]
  val critHitRate = 0.1       // in [0.0, 1.0]
  val type1 = Normal
  val power = 40
  val priority = 0
  val maxPP = 35
  var currentPP = maxPP

  def use(enemy: Pokemon, pb: Battle) = {
    println("POUND CALLED!")
    println(s"POUND's Pokemon = $pokemon")
    pokemon match {
      case None => { println("POUND MATCHED NONE!") }
      case Some(p) => {
        val effectiveAccuracy = accuracy      // TODO: take battle accuracy/evasion stages into account
        if (Random.nextDouble < effectiveAccuracy) {
          println("GOING AHEAD WITH POUND!")
        
          // http://bulbapedia.bulbagarden.net/wiki/Damage_modification#Damage_formula
          val baseAttack = p.attack
          val baseDefense = enemy.defense
          val effectiveAttack = baseAttack    // TODO: take battle stages into account, and BRN, and Reflect + Light Screen, and others
          val effectiveDefense = baseDefense  // TODO: take battle stages into account
          val base = power
          val typeMult = DamageCalculator.calculateTypeMultiplier(this, enemy)
          val STAB = if (type1 == p.type1 || type1 == p.type2) 1.5 else 1.0
          val critical = if (Random.nextFloat() < critHitRate) 2 else 1   // TODO: actually implement critHit logic
          val r = 0.85 + Random.nextDouble * (1.0 - 0.85)  // random between 0.85 and 1.00
        
          val A = (2 * p.level.toDouble + 10) / 250
          val B = effectiveAttack.toDouble / effectiveDefense
          val modifier = STAB * typeMult * critical * r
        
          val damage = ((A * B * base + 2) * modifier).toInt
          println(s"damage = $damage")
          enemy.takeDamage(damage)
        }      
      }
    }
  }
}
  
class NoMove(override val pokemon : Option[Pokemon]) extends Move(pokemon) {
  val index = -1
  val accuracy = 0.0
  val critHitRate = 0.0
  val type1 = Normal
  val power = 0
  val priority = 0
  val maxPP = 35
  var currentPP = 0

  override def use(enemy: Pokemon, pb: Battle) = {}
}

class Struggle(override val pokemon : Option[Pokemon]) extends Move(pokemon) {
  val index = 165
  val accuracy = 0.0
  val critHitRate = 0.0
  val type1 = Normal
  val power = 0
  val priority = 0
  val maxPP = 35
  var currentPP = 0

  override def use(enemy: Pokemon, pb: Battle) = {
    // Attack!
    
    // Take 1/2 damage dealt as recoil
  }
}
