package pokemodel

import scala.util.Random
import CritHitType._
import MoveType._

class DamageCalculator {
  /*
   * The damage that a move does is always captured by the logic in damageFormula
   * However, depending on whether or not the move lands a critical hit,
   * inputs to damageFormula change. The key method here, calc, dispatches to get
   * the appropriate inputs depending on whether or not a critical happened
   */
  
  def calc(attacker : Pokemon, defender : Pokemon, move : Move, battle : Battle) : Int = {
    // The key method, through which all damages are calculated
    val criticalChance = calcCriticalChance(attacker, defender, move, battle)
    if (Random.nextDouble < criticalChance) {
      calcCriticalHitDamage(attacker, defender, move, battle)
    } else {
      calcRegularHitDamage(attacker, defender, move, battle)
    }
  }
  
  private def damageFormula(level: Int, attack: Int, defense: Int, base: Int, mod: Double) : Int = {
    val A = (2 * level.toDouble + 10) / 250
    val B = attack.toDouble / defense
    ((A * B * base + 2) * mod).toInt
  }
  
  private def calcModifier(attacker: Pokemon, defender: Pokemon, move: Move) : Double = {
    // "Modifier" value in http://bulbapedia.bulbagarden.net/wiki/Damage_modification#Damage_formula
    // Used in both calcRegularHit and calcCriticalHit
    val typeMult = calculateTypeMultiplier(move.type1, defender)
    val STAB = stabBonus(attacker, move)
    val r = 0.85 + Random.nextDouble * (1.0 - 0.85)
    STAB * typeMult * r    
  }
  
  private def calcRegularHitDamage(attacker : Pokemon, defender : Pokemon, move : Move, battle : Battle) : Int = {
    // http://bulbapedia.bulbagarden.net/wiki/Damage_modification#Damage_formula
    val modifier = calcModifier(attacker, defender, move)
    val effectiveAttack = move.moveType match {
      case PHYSICAL => battle.statManager.getEffectiveAttack(attacker)
      case SPECIAL  => battle.statManager.getEffectiveDefense(attacker)
      case STATUS => throw new Exception("A Status move called calcRegularHitDamage!")
    }
    val effectiveDefense = move.moveType match {
      case PHYSICAL => battle.statManager.getEffectiveSpecial(attacker)
      case SPECIAL  => battle.statManager.getEffectiveSpecial(attacker)
      case STATUS => throw new Exception("A Status move called calcRegularHitDamage!")
    }
    val damage = damageFormula(attacker.level, effectiveAttack, effectiveDefense, move.power, modifier)
    damage
  }
  
  private def calcCriticalHitDamage(attacker : Pokemon, defender : Pokemon, move : Move, battle : Battle) : Int = {
    /* 
     * http://bulbapedia.bulbagarden.net/wiki/Critical_hit
     * - Attacker's level is temporarily doubled for damage calculation
     * - Ignore halved attack from BRN
     * - Ignore all stat mods, even beneficial ones
     */
    val modifier = calcModifier(attacker, defender, move)
    val attack = move.moveType match {
      case PHYSICAL => attacker.attack
      case SPECIAL  => attacker.defense
      case STATUS => throw new Exception("A Status move called calcCriticalHitDamage!")
    }
    val defense = move.moveType match {
      case PHYSICAL => attacker.special
      case SPECIAL  => attacker.special
      case STATUS => throw new Exception("A Status move called calcCriticalHitDamage!")
    }
    val damage = damageFormula(attacker.level * 2, attack, defense, move.power, modifier)
    damage
  }

  
  private def calcCriticalChance(attacker : Pokemon, defender : Pokemon, move : Move, battle : Battle) : Double = {
  // TODO: take FocusEnergy status (and Battle.focusEnergyHelps) into account when calculating criticalChance
    val criticalChance = move.critHitRate match {
      case LOW  => PokeData.getBaseSpeed(attacker.index).toDouble / 512
      case HIGH => PokeData.getBaseSpeed(attacker.index).toDouble / 64
    }
    criticalChance
  }
   
  private def calculateTypeMultiplier(attackType : Type.Value, d : Pokemon) : Double = {
    val dType1 = d.type1
    val dType2 = d.type2
    val m = Type.typeMap(attackType)
    if (dType1 == dType2) m(dType1)
    else m(dType1) * m(dType2)
  }
  
  private def stabBonus(attacker : Pokemon, move : Move) : Double = {
    if (attacker.type1 == move.type1 || attacker.type2 == move.type1) 1.5 
    else 1.0
  }
}