package pokemodel

import scala.util.Random
import CritHitType._
import Battle.{verbose=>VERBOSE}

class DamageCalculator {
  /*
   * The damage that a move does is always captured by the logic in
   * damageFormula However, depending on whether or not the move lands a
   * critical hit, inputs to damageFormula change. The key method here, calc,
   * figures out if a crithit occurred, comes up with the appropriate stat
   * values either way, and computes damage
   *
   * In both branches of calc, the full damage calculation is done, but then
   * the result is truncated so that it doesn't exceed the defender's
   * currentHP. This is the correct behavior: MoveResult.damageDealt value
   * should reflect how much damage is actually done to the Pokemon, rather
   * than just hitting him with all of it and letting Pokemon.takeDamage
   * truncate it. (For example, if you use a Recoil move, the attacking Pokemon
   * should take (usually) 25% of the damage ACTUALLY DEALT to the Pokemon. If
   * the move would have dealt 60 damage, but the defender only had 4 HP, then
   * the attacker should take 1 damage instead of 15 damage. So having a
   * correct value for damageDealt is the way to go
   */

  def calc(attacker: Pokemon, defender: Pokemon, move: Move, battle: Battle) = {
    // The key method, through which all damages are calculated
    // Returns a partially-completed MoveResultBuilder

    val criticalChance = calcCriticalChance(attacker, defender, move, battle)

    val result = new MoveResultBuilder()
                    .moveIndex(move.index)
                    .numTimesHit(1)  // misses and multistrikes can change
                    .STAB(stabBonus(attacker, move) == 1.5)
                    .moveType(move.type1)
                    .typeMult(calculateTypeMultiplier(move.type1, defender))

    def processRawDamage(rd: Int, r: MoveResultBuilder, d: Pokemon) {
      // Update MRB r with various damage values, based on raw damage rd
      // Used in both CritHit and RegHit branches below
      r.rawDamage(rd)
      r.damageDealt(rd min d.currentHP(false))
      r.dUnderlying(rd min d.currentHP(true))
    }

    if (Random.nextDouble < criticalChance) {
      result.critHit(true)
      val rawCritDam = calcCriticalHitDamage(attacker, defender, move, battle)
      processRawDamage(rawCritDam, result, defender)
    } else {
      val rawRegDam = calcRegularHitDamage(attacker, defender, move, battle)
      processRawDamage(rawRegDam, result, defender)
    }
    result
  }


  /* DC HELPER FUNCTIONS */
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
   * There doesn't seem to be a clear consensus on what the damage formula for
   * Gen 1 actually is...
   *
   * From http://wiki.pokemon-online.eu/view/Damage_formula
   * ((((Level * 0.4 * C) + 2) * Attack * movePower / 50 / Defense) + 2) *
   *   STAB*TypeModifier * random[217,255] / 255
   * C = 2 if criticalHit else 1
   * But crithits in Gen 1 seem to double the effective level of the attacker,
   * and NOT just double the damage
   *
   * From http://www.math.miami.edu/~jam/azure/compendium/battdam.htm:
   * ((0.4 * level + 2)*Attack*movePower)/Defense)/50)+2)*
   *     STAB)*TypeModifier/10)*random[217,255])/255
   * This isn't even close to having balanced parentheses, but at least it
   * seems to use the right stuff
   *
   * From http://www.serebii.net/games/damage.shtml:
   * ((((2 * Level / 5 + 2) * AttackStat * AttackPower / DefenseStat) / 50) + 2) *
   *     STAB * Weakness/Resistance * Random[85,100] / 100
   * This third one parses, and uses the correct stuff, and seems legit, so
   * that's what I'll do
   *
   * Also, according to the math.miami source, you have to truncate to an Int
   * every step of the way.  I ended up looking at the Javascript code of the
   * math.miami calculator and sort of implemented it here
   *
   * Note: In this formula, attack and defense are the EFFECTIVE attack and
   * defense, which depend on things like battle stat mods, statuses (BRN
   * halves attack, for example), and whether the move is Physical or Special.
   * Rather than worrying about all that here, we'll capture the formula here
   * and make sure that everything is accounted for elsewhere.
   */
  def damageFormula(level: Int,
                    effectiveAttack: Int,
                    effectiveDefense: Int,
                    basePower: Int,
                    stabBonus: Double,
                    typeEffectiveness: Double,
                    r: Double = (Utils.intBetween(85,101).toDouble / 100)): Int = {
    val q1: Int = (2 * level.toDouble / 5).toInt + 2
    val q2: Int = q1 * effectiveAttack * basePower
    val q3: Int = (q2.toDouble / effectiveDefense).toInt
    val q4: Int = (q3.toDouble / 50).toInt + 2
    val q5: Int = (q4 * stabBonus).toInt
    val q6: Int = (q5 * typeEffectiveness).toInt
    val result = q6 * r
    result.toInt
  }

  private def calcRegularHitDamage(
    attacker: Pokemon,
    defender: Pokemon,
    move: Move,
    battle: Battle): Int = {

    // http://bulbapedia.bulbagarden.net/wiki/Damage_modification#Damage_formula
    val effectiveAttack = move match {
      case (_: PhysicalMove) =>
        battle.statManager.getEffectiveAttack(attacker)
      case (_: SpecialMove) =>
        battle.statManager.getEffectiveSpecialAttack(attacker)
      case (_: StatusMove)  =>
        throw new Exception("A Status move called calcRegularHitDamage!")
    }

    val effectiveDefense = move match {
      case (_: PhysicalMove) =>
        battle.statManager.getEffectiveDefense(defender, battle)
      case (_: SpecialMove) =>
        battle.statManager.getEffectiveSpecialDefense(defender, battle)
      case (_: StatusMove) =>
          throw new Exception("A Status move called calcRegularHitDamage!")
    }

    val STAB = stabBonus(attacker, move)
    val typeMod = calculateTypeMultiplier(move.type1, defender)
    damageFormula(attacker.level, effectiveAttack, effectiveDefense, move.power,
        STAB, typeMod)
  }

  private def calcCriticalHitDamage(
    attacker: Pokemon,
    defender: Pokemon,
    move: Move,
    battle: Battle): Int = {

    /*
     * http://bulbapedia.bulbagarden.net/wiki/Critical_hit
     * - Attacker's level is temporarily doubled for damage calculation
     * - Ignore halved attack from BRN
     * - Ignore all stat mods, even beneficial ones, for both Pokemon
     */
    val attack = move match {
      case (_: PhysicalMove) => attacker.attack
      case (_: SpecialMove) => attacker.special
      case (_: StatusMove) =>
        throw new Exception("A Status move called calcCriticalHitDamage!")
    }
    val defense = move match {
      case (_: PhysicalMove) => defender.defense
      case (_: SpecialMove) => defender.special
      case (_: StatusMove) =>
        throw new Exception("A Status move called calcCriticalHitDamage!")
    }
    val STAB = stabBonus(attacker, move)
    val typeMod = calculateTypeMultiplier(move.type1, defender)
    damageFormula(attacker.level * 2, attack, defense, move.power, STAB, typeMod)
  }


  private def calcCriticalChance(attacker: Pokemon,
      defender: Pokemon,
      move: Move,
      battle: Battle): Double = {

    val criticalChance = move.critHitRate match {
      case NEVER  => 0.0   // for testing purposes
      case LOW    => PokeData.getBaseSpeed(attacker.index).toDouble / 512
      case HIGH   => PokeData.getBaseSpeed(attacker.index).toDouble / 64
      case ALWAYS => 1.0
    }

    if (battle.weirdMoveStatusManager.hasFocusEnergy(attacker)) {
      if (Glitch.focusEnergyGlitch) criticalChance / 4
      else criticalChance * 4
    } else criticalChance
  }
}
