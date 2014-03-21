package pokemodel

import scala.util.Random
import CritHitType._
import MoveType._

class DamageCalculator {
  
  // TODO: take status stuff like FocusEnergy into account when calculating criticalChance 
  def calc(attacker : Pokemon, 
           defender : Pokemon,
           move : Move,
           battle : Battle) : Int = {
    // http://bulbapedia.bulbagarden.net/wiki/Damage_modification#Damage_formula
    // http://bulbapedia.bulbagarden.net/wiki/Critical_hit
    val typeMult = calculateTypeMultiplier(move.type1, defender)
    val STAB = stabBonus(attacker, move)
    val criticalChance = move.critHitRate match {
      case LOW  => PokeData.getBaseSpeed(attacker.index).toDouble / 512
      case HIGH => PokeData.getBaseSpeed(attacker.index).toDouble / 64
    }
    val r = 0.85 + Random.nextDouble * (1.0 - 0.85)  // random between 0.85 and 1.00
    val modifier = STAB * typeMult * r  // same for both critHit and not
    
    if (Random.nextDouble < criticalChance) {
      /* Critical hit!
       * - Attacker's level is temporarily doubled for damage calculation
       * - Ignore halved attack from BRN
       * - Ignore all stat mods, even beneficial ones
       */
      val attack = move.moveType match {
        case PHYSICAL => attacker.attack
        case SPECIAL  => attacker.defense
        case STATUS => throw new Exception("A Status move called DamageCalculator.calc!")
      }
      val defense = move.moveType match {
        case PHYSICAL => attacker.special
        case SPECIAL  => attacker.special
        case STATUS => throw new Exception("A Status move called DamageCalculator.calc!")
      }
      val damage = calcHelper(attacker.level * 2, attack, defense, move.power, modifier)
      damage
    } else {
      // No critical hit
      val effectiveAttack = move.moveType match {
        case PHYSICAL => battle.statManager.getEffectiveAttack(attacker)
        case SPECIAL  => battle.statManager.getEffectiveDefense(attacker)
        case STATUS => throw new Exception("A Status move called DamageCalculator.calc!")
      }
      val effectiveDefense = move.moveType match {
        case PHYSICAL => battle.statManager.getEffectiveSpecial(attacker)
        case SPECIAL  => battle.statManager.getEffectiveSpecial(attacker)
        case STATUS => throw new Exception("A Status move called DamageCalculator.calc!")
      }
      val damage = calcHelper(attacker.level, effectiveAttack, effectiveDefense, move.power, modifier)
      damage
    }
  }
  
  private def calcHelper(level: Int, attack: Int, defense: Int, base: Int, mod: Double) : Int = {
    val A = (2 * level.toDouble + 10) / 250
    val B = attack.toDouble / defense
    ((A * B * base + 2) * mod).toInt
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