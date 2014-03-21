package pokemodel

import scala.util.Random
import CritHitType._

class DamageCalculator {
  def criticalHit(m : Move) : Boolean = false
  
  def calc(attacker : Pokemon, 
           defender : Pokemon,
           effectiveAttack : Int,
           effectiveDefense : Int,
           move : Move) : Int = {
    // http://bulbapedia.bulbagarden.net/wiki/Damage_modification#Damage_formula
    // http://bulbapedia.bulbagarden.net/wiki/Critical_hit
    val typeMult = calculateTypeMultiplier(move.type1, defender)
    val STAB = stabBonus(attacker, move)
    val criticalChance = move.critHitRate match {
      case LOW  => PokeData.getBaseSpeed(attacker.index).toDouble / 512
      case HIGH => PokeData.getBaseSpeed(attacker.index).toDouble / 512
    }
    
    if (Random.nextDouble < criticalChance) {
      /* Critical hit!
       * - Attacker's level is temporarily doubled for damage calculation
       * - Ignore halved attack from BRN
       * - Ignore all stat mods, even beneficial ones
       * 
       */
    }
    val r = 0.85 + Random.nextDouble * (1.0 - 0.85)  // random between 0.85 and 1.00
    
    val A = (2 * attacker.level.toDouble + 10) / 250
    val B = effectiveAttack.toDouble / effectiveDefense
    val modifier = STAB * typeMult * r
    
    val damage = ((A * B * move.power + 2) * modifier).toInt
    println(s"Damage = $damage from $this")
    damage
  }
  
  def calculateTypeMultiplier(attackType : Type.Value, d : Pokemon) : Double = {
    val dType1 = d.type1
    val dType2 = d.type2
    val m = Type.typeMap(attackType)
    if (dType1 == dType2) m(dType1)
    else m(dType1) * m(dType2)
  }
  
  def stabBonus(attacker : Pokemon, move : Move) : Double = {
    if (attacker.type1 == move.type1 || attacker.type2 == move.type1) 1.5 
    else 1.0
  }
}