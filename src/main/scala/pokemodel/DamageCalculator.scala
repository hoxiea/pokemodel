package pokemodel

import scala.util.Random
import CritHitType._
import MoveType._
import Battle.{verbose=>VERBOSE}

class DamageCalculator {
  /*
   * The damage that a move does is always captured by the logic in damageFormula
   * However, depending on whether or not the move lands a critical hit,
   * inputs to damageFormula change. The key method here, calc, figures out if a crithit
   * occurred, comes up with the appropriate stat values either way, and computes damage
   */

  def calc(attacker: Pokemon, defender: Pokemon, move: Move, battle: Battle): Int = {
    // The key method, through which all damages are calculated
    val criticalChance = calcCriticalChance(attacker, defender, move, battle)
    if (Random.nextDouble < criticalChance) {
      if (VERBOSE) println(s"Critical hit for $move")
      calcCriticalHitDamage(attacker, defender, move, battle)
    } else {
      calcRegularHitDamage(attacker, defender, move, battle)
    }

  }

  /* HELPER FUNCTIONS */
  def stabBonus(attacker: Pokemon, move: Move): Double = {
    if (attacker.type1 == move.type1 || attacker.type2 == move.type1) 1.5
    else 1.0
  }

  def calculateTypeMultiplier(attackType: Type.Value, d: Pokemon): Double = {
    val dType1 = d.type1
    val dType2 = d.type2
    val m = Type.typeMap(attackType)
    if (dType1 == dType2) m(dType1)
    else m(dType1) * m(dType2)
  }

  /*
   * There doesn't seem to be a clear consensus on what the damage formula for Gen 1 actually is...
   *
   * From http://wiki.pokemon-online.eu/view/Damage_formula
   * ((((Level * 0.4 * C) + 2) * Attack * movePower / 50 / Defense) + 2) * STAB*TypeModifier * random[217,255] / 255
   * C = 2 if criticalHit else 1
   * But crithits in Gen 1 seem to double the effective level of the attacker, and NOT just double the damage
   *
   * From http://www.math.miami.edu/~jam/azure/compendium/battdam.htm:
   * ((0.4 * level + 2)*Attack*movePower)/Defense)/50)+2)*STAB)*TypeModifier/10)*random[217,255])/255
   * This isn't even close to having balanced parentheses, but at least it seems to use the right stuff
   *
   * From http://www.serebii.net/games/damage.shtml:
   * ((((2 * Level / 5 + 2) * AttackStat * AttackPower / DefenseStat) / 50) + 2) * STAB * Weakness/Resistance * Random[85,100] / 100
   * This third one parses, and uses the correct stuff, and seems legit, so that's what I'll do
   *
   * Note: In this formula, attack and defense are the EFFECTIVE attack and defense, which depend on things
   * like battle stat mods, statuses (BRN halves attack, for example), and whether the move is Physical or
   * Special. Rather than worrying about all that here, we'll capture the formula here and make sure that
   * everything is accounted for elsewhere.
   *
   * Also, according to the math.miami source, you have to truncate to an Int every step of the way.
   * But that makes things really dependent on the order you do them. So instead, I kept everything a
   * Double, let decimals accumulate, and truncate at the end. More computationally efficient too.
   */
  def damageFormula(level: Int,
                    effectiveAttack: Int,
                    effectiveDefense: Int,
                    basePower: Int,
                    allTypeStuff: Double,
                    r: Double = (Utils.intBetween(85,101).toDouble / 100)): Int = {
    val q1 = (2 * level.toDouble / 5) + 2
    val q2 = (q1 * effectiveAttack * basePower) / effectiveDefense
    val q3 = q2 / 50
    val q4 = (q3 + 2)
    val result = (q4 * allTypeStuff * r)
    println(s"q1 = $q1")
    println(s"q2 = $q2")
    println(s"q3 = $q3")
    println(s"q4 = $q4")
    println(s"result = $result")
    result.toInt
  }

  def calcModifier(attacker: Pokemon,
                   defender: Pokemon,
                   move: Move): Double = {
    // Used in both calcRegularHit and calcCriticalHit
    val typeMult = calculateTypeMultiplier(move.type1, defender)
    val STAB = stabBonus(attacker, move)
    STAB * typeMult
  }

  def calcRegularHitDamage(attacker: Pokemon, defender: Pokemon, move: Move, battle: Battle): Int = {
    // http://bulbapedia.bulbagarden.net/wiki/Damage_modification#Damage_formula
    val modifier = calcModifier(attacker, defender, move)
    val effectiveAttack = move.moveType match {
      case PHYSICALMOVE => battle.statManager.getEffectiveAttack(attacker)
      case SPECIALMOVE  => battle.statManager.getEffectiveDefense(attacker)
      case STATUSMOVE   => throw new Exception("A Status move called calcRegularHitDamage!")
    }
    val effectiveDefense = move.moveType match {
      case PHYSICALMOVE => battle.statManager.getEffectiveSpecial(attacker)
      case SPECIALMOVE  => battle.statManager.getEffectiveSpecial(attacker)
      case STATUSMOVE   => throw new Exception("A Status move called calcRegularHitDamage!")
    }
    println(s"effectiveAttack for $move = $effectiveAttack")
    println(s"effectiveDefense against $move = $effectiveDefense")
    val damage = damageFormula(attacker.level, effectiveAttack, effectiveDefense, move.power, modifier)
    damage
  }

  def calcCriticalHitDamage(attacker: Pokemon, defender: Pokemon, move: Move, battle: Battle): Int = {
    /*
     * http://bulbapedia.bulbagarden.net/wiki/Critical_hit
     * - Attacker's level is temporarily doubled for damage calculation
     * - Ignore halved attack from BRN
     * - Ignore all stat mods, even beneficial ones, for both Pokemon
     */
    val modifier = calcModifier(attacker, defender, move)
    val attack = move.moveType match {
      case PHYSICALMOVE => attacker.attack
      case SPECIALMOVE  => attacker.defense
      case STATUSMOVE   => throw new Exception("A Status move called calcCriticalHitDamage!")
    }
    val defense = move.moveType match {
      case PHYSICALMOVE => attacker.special
      case SPECIALMOVE  => attacker.special
      case STATUSMOVE   => throw new Exception("A Status move called calcCriticalHitDamage!")
    }
    val damage = damageFormula(attacker.level * 2, attack, defense, move.power, modifier)
    damage
  }


  def calcCriticalChance(attacker: Pokemon, defender: Pokemon, move: Move, battle: Battle): Double = {
  // TODO: take FocusEnergy status (and Battle.focusEnergyHelps) into account when calculating criticalChance
    val criticalChance = move.critHitRate match {
      case LOW  => PokeData.getBaseSpeed(attacker.index).toDouble / 512
      case HIGH => PokeData.getBaseSpeed(attacker.index).toDouble / 64
    }
    criticalChance
  }

}
