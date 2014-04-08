package pokemodel

import scala.collection.mutable
import Type._
import MoveType._
import BattleStat._
import CritHitType._
import TakeDamageResult._
import ViolentStruggleType._
import scala.util.Random
import Battle.{verbose=>VERBOSE}

/*
 * The Move hierarchy is as follows:
 *                      __Move__
 *                     /   |    \
 *                    /    |     \
 *                   /     |      \
 *           Physical   Special   Status
 *
 * Physical moves use the Attack stat of attacker and Defense stat of defender
 * Special moves use the Special stat of attacker and Special stat of defender
 * Status moves ignore stats and typically do pretty weird stuff
 *
 * There are 165 moves that Pokemon can learn and use in Generation 1.
 *
 * Many moves have things in common. For example, there are 21 Physical moves
 * and 5 Special moves that just deal damage. There are a bunch of both
 * Physical and Special moves that deal damage and, if they hit, have some
 * chance of causing a status ailment. There are also a bunch of both Physical
 * and Special moves that deal damage and, if they hit, have some chance of
 * causing a battlestat change. This suggests we should be able to use object-
 * oriented techniques to cut down on code reuse and increase maintainability.
 *
 * My inital attempt was to subclass Physical, Special, and Status into things
 * like PhysicalSingleStrike, SpecialSingleStrikeStatusChange, and so on. But
 * that led to code duplication: the SingleStrike logic appeared in
 * PhysicalSingleStrike, SpecialSingleStrike, SpecialSingleStrikeStatusChange,
 * and so on.
 *
 * I then stumbled upon the idea of stacking traits: for each behavior that I
 * needed to capture (SingleStrike, MultiStrike, StatusChange, OneHitKO, etc.),
 * I could encapsulate that behavior's logic in a single trait that all moves
 * of that type could mixin.
 *
 * This works quite elegantly, as long as you're careful about the order in
 * which you mix in traits. For example, every move that causes recoil damage
 * in this game has two common components to it: SingleStrike, and Recoil.
 * And every recoil move deals recoil damage as a function of the amount of
 * damage dealt to the other Pokemon. So before the Recoil trait can deduct
 * HP from the attacker, it has to know how much damage was dealt to the enemy.
 * That's why all recoil Moves defined in ActualMoves are of the forms:
 * class MyRecoilMove extends PhysicalMove with Recoil with SingleStrike
 * class MyRecoilMove extends SpecialMove  with Recoil with SingleStrike
 * The SingleStrike does its thing, records how much damage it dealt, and
 * then passes that left to Recoil, which can then act accordingly.
 *
 * As another example of trait ordering being important, consider something
 * like Thunder, which deals damage in one strike and has the chance of
 * paralyzing the enemy. The paralysis is only considered if the attack hits,
 * so SingleStrike should do its thing before StatusChange does, and StatusChange
 * needs to check and make sure that numTimesHit != 0 before even thinking about
 * generating a random number and causing PAR.
 *
 * Anyway, with a handful of traits that are arranged carefully, it's possible
 * to implement
 * TODO: how many moves are just clever combinations of traits?
 * out of 165 Moves with just a few lines of code each.
 *
 * This file starts by defining what a Move is. It then defines the three
 * subclasses in the diagram above, making sure to give default values as per
 * the Stacking Traits Pattern. And finally, a bunch of traits each capture one
 * piece of useful logic that multiple moves can take advantage of.
 *
 * For the actual instantiation of the 165 Moves, see ActualMoves.scala.
 * See also MoveMaker.scala.
 */

// TODO: Build some integrated way to figure out if a move hits that takes RNG, accuracy, attacker accuracy, evasion, statuses, Fly/Dig/Haze/etc. into account

/*
 * All Moves are a subclass of type Move, which defines what it means to be a move.
 * It captures the basic but necessary information about a move, and makes sure that
 * all Moves do the required set-up and clean-up by declaring #use to be final.
 *
 * Essentially, moves fill in their unique information (index number, type, power,
 * maxPP), override anything else unique about them (high critical hit rate, weird
 * priority, imperfect accuracy).
 *
 * Moves also fill in things that the specific traits that they mix in require:
 * a status to cause and the chance of causing it for StatusChange, the
 * proportion of damage dealt to incur in Recoil, etc.
 *
 * Finally, a move must implement moveSpecificStuff, which captures the behavior
 * of the move as it interacts with the active enemy and the battle. Many moves
 * have a common behavior that's totally taken care of by traits. Others are
 * unique and had to be written by hand.
 */
abstract class Move {
  val index : Int                // in 1 .. 165, plus Test Moves
  val type1 : Type.Value         // moves only have 1 type
  val moveType : MoveType.Value  // PHYSICAL, SPECIAL, or STATUS
  val power : Int                // base power of the move
  val maxPP : Int

  val critHitRate = LOW   // True for 99% of moves, outliers can override
  val priority = 0        // True for 99% of moves, outliers can override
  val accuracy = 1.0      // in [0.0, 1.0], true for ~60% of moves, others can override
  val mrb = new MoveResultBuilder()

  // Even moves with 100% accuracy might miss because of accuracy/evasion
  // adjustments in battle
  def chanceHit(attacker : Pokemon, defender : Pokemon, pb : Battle): Double = {
    val attackerAccuracy = pb.statManager.getEffectiveAccuracy(attacker).toDouble
    val defenderEvasion = pb.statManager.getEffectiveEvasion(defender)
    accuracy * attackerAccuracy / defenderEvasion
  }

  def startUsingMove(
      attacker: Pokemon,
      attackerMoveSlot: Int,
      defender: Pokemon,
      pb: Battle) {

    // Function called before move-specific stuff happens
  }

  // This is what each specific move is ultimately responsible for filling in
  // along with index and type and that stuff
  def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder): MoveResultBuilder


  /* Function called after move-specific stuff happens
   * This is nice because it gets the MRB from moveSpecificStuff and can
   * piggyback onto it even further if it wants to.
   * But by default, it just passed along what it received, to be converted
   * into a MoveResult and returned by 'use' below
   */
  def finishUsingMove(
      attacker: Pokemon,
      attackerMoveSlot: Int,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder): MoveResultBuilder = {

    pb.moveManager.updateLastMoveIndex(attacker, index)
    attacker.deductPP(attackerMoveSlot)
    mrb
  }

  final def use(
      attacker: Pokemon,
      attackerMoveSlot: Int,
      defender: Pokemon,
      pb: Battle): MoveResult = {

    startUsingMove(attacker, attackerMoveSlot, defender, pb)
    val mssMRB = moveSpecificStuff(attacker, defender, pb, mrb)
    val fumMRB = finishUsingMove(attacker, attackerMoveSlot, defender, pb, mssMRB)
    fumMRB.toMoveResult
  }

  override def toString() = {
    val moveName = this.getClass().getName()
    val prefix = "pokemodel."
    if (moveName.startsWith(prefix)) moveName.substring(prefix.length)
    else moveName
  }
}


/*
 * Below a Move in the class hierarchy are the three different types of Moves:
 * Physical, Special, and Status. These tack on a moveType value, which the
 * Battle's DamageCalculator uses to determine the correct stats to use.  They
 * also give default values to the abstract members of Move, so that the
 * stackable traits pattern will fly. Importantly, they convert the
 * MoveResultBuilder that's been passed up along the chain of traits and
 * convert it to a MoveResult, to be returned
 */
class PhysicalMove extends Move {
  override val index = 0
  override val type1 = Normal
  override val power = 0
  override val maxPP = 0
  override val moveType = PHYSICALMOVE
  override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder) = mrb
}

class SpecialMove extends Move {
  override val index = 0
  override val type1 = Normal
  override val power = 0
  override val maxPP = 0
  override val moveType = SPECIALMOVE
  override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder) = mrb
}

class StatusMove extends Move {
  override val index = 0
  override val type1 = Normal
  override val power = 0
  override val maxPP = 0
  override val moveType = STATUSMOVE
  override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder) = mrb
}


/*
 * These allow you to change the type of a move on the fly. For example,
 * val m1 = new TestPhysicalSingleStrike with Electric
 * val m2 = new TestPhysicalSingleStrike with PsychicT
 * Very useful for testing effectivenesses, etc.
 */
trait Normal extends Move   { override val type1 = Normal }
trait Fighting extends Move { override val type1 = Fighting }
trait Flying extends Move   { override val type1 = Flying }
trait Poison extends Move   { override val type1 = Poison }
trait Ground extends Move   { override val type1 = Ground }
trait Rock extends Move     { override val type1 = Rock }
trait Bug extends Move      { override val type1 = Bug }
trait Ghost extends Move    { override val type1 = Ghost }
trait Fire extends Move     { override val type1 = Fire }
trait Water extends Move    { override val type1 = Water }
trait Grass extends Move    { override val type1 = Grass }
trait Electric extends Move { override val type1 = Electric }
trait PsychicT extends Move  { override val type1 = Psychic }  // avoid conflict with move Psychic
trait Ice extends Move      { override val type1 = Ice }
trait Dragon extends Move   { override val type1 = Dragon }

// Likewise, handy ways to change the base power on the fly
trait Power40 extends Move  { override val power = 40 }
trait Power80 extends Move  { override val power = 80 }
trait Power120 extends Move { override val power = 120 }
trait Power160 extends Move { override val power = 160 }
trait Power200 extends Move { override val power = 200 }

// Quick way to get more critical hits, for testing purposes
trait NeverCritHit extends Move  { override val critHitRate = NEVER }
trait HighCritHit extends Move   { override val critHitRate = HIGH }
trait AlwaysCritHit extends Move { override val critHitRate = ALWAYS }

// High priority, for testing purposes
trait Priority1 extends Move  { override val priority = 1 }
trait Priority2 extends Move  { override val priority = 2 }

// Super useful for metronome
trait PriorityNormal extends Move  { override val priority = 0 }


/* Next, capture common behaviors of Moves - damage, stats, status, etc. */
trait SingleStrike extends Move {
  /*
   * A basic damage-dealing experience
   * Any move that strikes once and uses DamageCalculator to figure out the
   * appropriate damage should extend SingleStrike
   */
  def requiredNVSAs: Set[NonVolatileStatusAilment] = Set()

  abstract override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    def statusAilmentSituationOK: Boolean =
      requiredNVSAs.isEmpty ||
      defender.statusAilment.isDefined && requiredNVSAs.contains(defender.statusAilment.get)

    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender) &&
        statusAilmentSituationOK) {
      val result = pb.dc.calc(attacker, defender, this, pb)
      val damageResult = defender.takeDamage(result.damageDealt)

      // Update result values
      // numTimesHit == 1, no hpGained, no statusAilments, no stat changes
      result.processTakeDamageResult(defender, damageResult)

      // Combine anything passed in from traits further to the right
      result.merge(mrb)

      // Pass at all along to the next trait/class
      super.moveSpecificStuff(attacker, defender, pb, result)

    } else {
      // Pass along what you got + moveIndex + moveType
      val missResult = new MoveResultBuilder().moveIndex(index).moveType(type1)
      missResult.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, missResult)
    }
  }
}


trait ConstantDamage extends Move {
  // Deal a given amount of damage, ignoring type effectiveness, STAB, etc.
  def damageAmount: Int

  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    // In this case, we skip DamageCalculator and build a MRB from scratch
    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)

    // Add the effects of hitting if necessary
    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {

      result.damageCalc(damageAmount)
      val damageToDeal = damageAmount min defender.currentHP()
      val damageResult = defender.takeDamage(damageToDeal)

      result.numTimesHit(1)
      result.damageDealt(damageToDeal)

      // no crithits, STAB, moveType, typeMult, statusChange, statChange
      result.processTakeDamageResult(defender, damageResult)
    }

    // Merge with given stuff and pass it along
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}


trait Recoil extends Move {
  /*
   * Take damage equal to some proportion of the damage dealt to the opponent,
   * usually 25%. Because of the fact that the damage must be dealt before
   * recoil can figure out how much damage to deal, Recoil should only be mixed
   * in TO THE LEFT of SingleStrike
   *
   * Substitutes don't absorb recoil damage, so we bypass the substitute. And
   * since we're going to be dealing damage directly to the underlying Pokemon,
   * that's the quantity of HP we need to use to truncate
   */
  def recoilProportion: Double

  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val bypassSub = true
    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    result.merge(mrb)  // need to know how much damage was dealt; don't mutate mrb!
    val damageToTake = (result.damageDealt * recoilProportion).toInt min attacker.currentHP(bypassSub)
    val damageResult = attacker.takeDamage(damageToTake, bypassSub)
    assert(damageResult != SUBKO)  // recoil damage shouldn't touch sub!
    if (damageResult == KO) {
      result.selfKO(true)
      assert(!attacker.isAlive)
    }
    super.moveSpecificStuff(attacker, defender, pb, result)  // already merged
  }
}




trait MultiStrike extends Move {
  /* Hit the opponent 2, 3, 4, or 5 times
   * In Gen 1, the probabilities were (0.375, 0.375, 0.125, 0.125) respectively
   * Also in Gen 1, damage was calculated only once and then used for all
   * strikes.  So a critical hit on the first strike will cause critical damage
   * all the way through.
   */
  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {
      val r = Random.nextDouble
      val numStrikes = if (r <= 0.375) 2
                       else if (r <= (0.375 + 0.375)) 3
                       else if (r <= (0.375 + 0.375 + 0.125)) 4
                       else 5
      val result = pb.dc.calc(attacker, defender, this, pb)
      val damageEachStrike = result.damageDealt  // don't use damageCalc!

      // Figure out what sequence of damages we should actually deal
      // Pokemon.currentHP() returns the HP of the substitute if one exists,
      // and the HP of the Pokemon if there's no sub. Either way, damageSeq
      // is designed to stop early if it would kill the active thing
      val damageSeq = Utils.damageSeqCalc(numStrikes, damageEachStrike, defender.currentHP())

      // Actually deal the damage in damageSeq
      val takeDamageResultSeq = damageSeq.map(n => defender.takeDamage(n))

      // All that we really care about is the last MoveResult in resultSeq,
      // since that will have information about KO/subKO, damageDealt for
      // Bide/Counter, etc.
      val relevantDamageResult = takeDamageResultSeq.last

      // Update result with information from relevantTakeDamageResult
      result.numTimesHit(damageSeq.length)
      result.damageDealt(damageSeq.last)
      result.processTakeDamageResult(defender, relevantDamageResult)
      result.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, result)
    } else {
      // Miss, pass along what you got + moveIndex without mutating mrb
      val missResult = new MoveResultBuilder().moveIndex(index).moveType(type1)
      missResult.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, missResult)
    }
  }
}


trait DoubleStrike extends Move {
  // Very similar to MultiStrike, except it always hits exactly twice if it hits
  // Unless it breaks a substitute, in which case it stops immediately
  // Bide and Counter only acknowledge the last strike
  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {

      val numStrikes = 2
      val result = pb.dc.calc(attacker, defender, this, pb)
      val damageEachStrike = result.damageDealt
      val damageSeq = Utils.damageSeqCalc(numStrikes, damageEachStrike, defender.currentHP())

      // Actually deal the damage in damageSeq
      val takeDamageResultSeq = damageSeq.map(n => defender.takeDamage(n))

      // All that we really care about is the last MoveResult in resultSeq,
      // since that will have information about KO/subKO, damageDealt for
      // Bide/Counter, etc.
      val relevantDamageResult = takeDamageResultSeq.last

      // Update result with information from relevantTakeDamageResult
      result.numTimesHit(damageSeq.length)
      result.damageDealt(damageSeq.last)
      result.processTakeDamageResult(defender, relevantDamageResult)
      result.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, result)
    } else {
      // Pass along what you got + moveIndex
      val missResult = new MoveResultBuilder().moveIndex(index).moveType(type1)
      missResult.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, missResult)
    }
  }
}


trait SelfStatChange extends Move {
  /*
   * A trait for Moves that let you change your own battle stats.
   *
   * Example Moves: Sharpen, SwordsDance, Harden, Amnesia, etc.
   *
   * This is the simplest of the 4 stat/status change traits, because the move
   * always succeeds(1) and doesn't depend on the success of a SingleStrike or
   * anything like that. (Of course, that's only because there aren't any moves
   * in Gen 1 that simultaneously deal damage AND have a chance of improving
   * your battle stats, but still.)
   *
   * (1): subject to logic about changing your own stats in BattleStatManager
   */

  def statToChange: BattleStat
  def amountToChangeBy: Int

  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    if (pb.statManager.canChangeOwnStats(attacker, pb)) {
      statToChange match {
        case ATTACK   => pb.statManager.changeAttackStage(attacker, amountToChangeBy)
        case DEFENSE  => pb.statManager.changeDefenseStage(attacker, amountToChangeBy)
        case SPEED    => pb.statManager.changeSpeedStage(attacker, amountToChangeBy)
        case SPECIAL  => pb.statManager.changeSpecialStage(attacker, amountToChangeBy)
        case ACCURACY => pb.statManager.changeAccuracyStage(attacker, amountToChangeBy)
        case EVASION  => pb.statManager.changeEvasionStage(attacker, amountToChangeBy)
      }
      result.addSelfStat(statToChange, amountToChangeBy)
    }

    // Pass along what you got + moveIndex
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}


trait EnemyStatChange extends Move {
  /*
   * A trait for moves that let you change your opponent's battle stats.
   *
   * There are two situations in which this can happen:
   * 1. Moves whose sole purpose it is to do so.
   *    Examples: TailWhip, SandAttack, StringShot, Growl, Leer, Kinesis, etc.
   *    These moves have an accuracy and use the standard accuracy formula to
   *    determine hit/miss. If they hit, they have a 100% chance of causing the
   *    stat change.
   *
   * 2. Moves that combine SingleStrike with a small chance of changing a stat.
   *    Examples: AuroraBeam, Acid, Psychic, Bubble, Bubblebeam
   *    These Moves should only consider causing the status change if
   *    a) The SingleStrike portion of the Move didn't kill the enemy
   *    b) The SingleStrike portion hit
   *    These moves have a <100% chance of causing the stat change; the actual
   *    chance is specified in chanceOfStatChange below. Also, SingleStrike must
   *    be mixed in to the right, so that a hit can be verified by enemyStatChange
   *
   * The flag soloStatChange controls which case is used.
   * - true  => Type 1. In every type 1 case, chanceOfStatChange should be 1.0
   * - false => Type 2. Check for hit && KO before proceeding, probably with
   *                    chanceOfStatChange < 1.0
   */

  def statToChange: BattleStat
  def amountToChangeBy: Int
  def chanceOfStatChange: Double
  def soloStatChange: Boolean

  def statChangeHits: Boolean = Random.nextDouble < chanceOfStatChange

  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    if (soloStatChange && chanceOfStatChange < 1.0) {
      throw new Exception("soloStatChange + chanceOfStatChange combo")
    }

    val proceedType1 =
      soloStatChange && Random.nextDouble < chanceHit(attacker, defender, pb)
    // could tack '&& statChangeHits' on end, but chanceOfStatChange was
    // verified to be 1.0 in this case

    val proceedType2 = !soloStatChange && !mrb.KO && mrb.numTimesHit > 0 && statChangeHits

    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    if (pb.statManager.canChangeDefenderStats(attacker, defender, pb) &&
        (proceedType1 || proceedType2)) {
      statToChange match {
        case ATTACK   => pb.statManager.changeAttackStage(defender, amountToChangeBy)
        case DEFENSE  => pb.statManager.changeDefenseStage(defender, amountToChangeBy)
        case SPEED    => pb.statManager.changeSpeedStage(defender, amountToChangeBy)
        case SPECIAL  => pb.statManager.changeSpecialStage(defender, amountToChangeBy)
        case ACCURACY => pb.statManager.changeAccuracyStage(defender, amountToChangeBy)
        case EVASION  => pb.statManager.changeEvasionStage(defender, amountToChangeBy)
      }
      result.addEnemyStat(statToChange, amountToChangeBy)
      result.numTimesHit(1)   // for Type1 situations where no hit previously registered
    }

    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}


trait NonVolatileStatusChange extends Move {
  /*
   * Cause some kind of NonVolatileStatusAilment (NVSA) to the opponent.
   * See StatusAilment.scala to understand volatile vs. non-volatile.
   *
   * Like EnemyStatChange, there are two use cases:
   * 1. Moves whose sole purpose is to cause a NVSA.
   *    Examples: = ThunderWave, StunSpore, Glare, SleepPowder, etc.
   *    These moves have an accuracy and use the standard accuracy formula to
   *    determine hit/miss. If they hit, they have a 100% chance of causing the
   *    status change.
   *
   * 2. Moves that combine SingleStrike with a small chance of a status change.
   *    Examples: Thunderbolt, Flamethrower, Sludge, Blizzard
   *    These Moves should only consider causing the status change if:
   *    a) The SingleStrike portion of the Move didn't kill the enemy
   *    b) The SingleStrike portion hit
   *    These moves have a <100% chance of causing the stat change; the actual
   *    chance is specified in chanceOfCausingAilment below. Also, SingleStrike must
   *    be mixed in to the right, so that a hit can be verified by NVSA
   *
   * The flag soloStatusChange controls which case is used.
   * - true  => Type 1. In every Type 1 case, chanceOfCausingAilment should be 1.0
   * - false => Type 2. Check for hit && KO before proceeding, probably with
   *                    chanceOfStatChange < 1.0
   */

  def statusAilmentToCause : NonVolatileStatusAilment
  def chanceOfCausingAilment : Double
  def statusAilmentCaused : Boolean = Random.nextDouble < chanceOfCausingAilment
  def soloStatusChange : Boolean
  def worksWhenSubPresent : Boolean  // Sleep Powder is the only move with false

  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    if (soloStatusChange && chanceOfCausingAilment < 1.0)
      throw new Exception("soloStatusChange + chanceOfCausingAilment combo")

    val proceedType1 =
      soloStatusChange && Random.nextDouble < chanceHit(attacker, defender, pb)
    val proceedType2 =
      !soloStatusChange && !mrb.KO && mrb.numTimesHit > 0 && statusAilmentCaused

    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    if ((proceedType1 || proceedType2) &&
      pb.statusManager.changeMajorStatusAilment(defender, statusAilmentToCause)) {
      result.nvsa(statusAilmentToCause)
      result.numTimesHit(1)  // for Type1 situations

      // TODO: if defender has HyperBeam delay, then getting put to SLP will
      // negate that delay
    }

    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}



trait VolatileStatusChange extends Move {
  /*
   * Cause some kind of VolatileStatusAilment (VSA) to the opponent.
   * See StatusAilment.scala to understand volatile vs. non-volatile.
   *
   * Like EnemyStatChange and NVSA, there are two use cases:
   * 1. Moves whose sole purpose is to cause a NVSA.
   *    Examples: = ConfuseRay, Supersonic
   *    These moves have an accuracy and use the standard accuracy formula to
   *    determine hit/miss. If they hit, they have a 100% chance of causing the
   *    status change.
   *
   * 2. Moves that combine SingleStrike with a small chance of a status change.
   *    Examples: Confusion, Psybeam
   *    These Moves should only consider causing the status change if:
   *    a) The SingleStrike portion of the Move didn't kill the enemy
   *    b) The SingleStrike portion hit
   *    These moves have a <100% chance of causing the stat change; the actual
   *    chance is specified in chanceOfCausingAilment below. Also, SingleStrike must
   *    be mixed in to the right, so that a hit can be verified by NVSA
   *
   * The flag soloStatusChange controls which case is used.
   * - true  => Type 1. In every Type 1 case, chanceOfCausingAilment should be 1.0
   * - false => Type 2. Check for hit && KO before proceeding, probably with
   *                    chanceOfStatChange < 1.0
   *
   * Unique to VolatileStatusAilment moves is that for some reason, some of them
   * claim to fail when a substitute is present. Whether or not your move works
   * should be captured in the worksWhenSubPresent variable.
   */

  def statusAilmentToCause   : VolatileStatusAilment
  def chanceOfCausingAilment : Double
  def soloStatusChange : Boolean
  def worksWhenSubPresent: Boolean

  def statusAilmentCaused: Boolean = Random.nextDouble < chanceOfCausingAilment

  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    if (soloStatusChange && chanceOfCausingAilment < 1.0)
      throw new Exception("soloStatusChange + chanceOfCausingAilment combo")

    val proceedType1 =
      soloStatusChange && Random.nextDouble < chanceHit(attacker, defender, pb)
    val proceedType2 =
      !soloStatusChange && !mrb.KO && mrb.numTimesHit > 0 && statusAilmentCaused

    val subSituationOK = worksWhenSubPresent || !defender.hasSub

    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    if (subSituationOK && (proceedType1 || proceedType2)) {
      statusAilmentToCause match {
        case (_ : CONFUSED) => {
          if (pb.statusManager.tryToCauseConfusion(defender)) {
            result.vsa(statusAilmentToCause)
            result.numTimesHit(1)
          }
        }
        case (_ : FLINCH) => {
          if (pb.statusManager.causeToFlinch(defender)) {
            // TODO: if defender has HyperBeam delay, then getting hit with a flinching
            // move will negate that delay
            result.vsa(statusAilmentToCause)
            result.numTimesHit(1)
          }
        }
        case (_ : PARTIALLYTRAPPED) => {
          if (pb.statusManager.tryToPartiallyTrap(defender)) {
            result.vsa(statusAilmentToCause)
            result.numTimesHit(1)
          }
        }
        case (_ : SEEDED) => {
          if (pb.statusManager.tryToSeed(defender)) {
            result.vsa(statusAilmentToCause)
            result.numTimesHit(1)
          }
        }
      }
    }
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}


/** Less Commonly-Used (but still code-saving) Traits **/
trait OneHitKO extends Move {
  /*
   * There are few OneHitKO moves in the game: Fissure, HornDrill, and Guillotine
   * As their name suggests, they kill their opponent if they connect.
   * More specifically, if they connect, then they'll either break a substitute if one exists,
   * or KO the enemy if a substitute doesn't exist.
   *
   * In Gen 1, these never succeeded against enemies with larger effective Speeds,
   * so we check for that when determining if the move strikes.
   */
  abstract override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    def attackerFastEnough: Boolean =
      pb.statManager.getEffectiveSpeed(attacker) >= pb.statManager.getEffectiveSpeed(defender)

    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    if (Random.nextDouble < chanceHit(attacker, defender, pb)
        && pb.statusManager.canBeHit(defender)
        && attackerFastEnough) {
      val damageToDeal = defender.currentHP()
      val damageResult = defender.takeDamage(damageToDeal)

      result.damageCalc(damageToDeal)
      result.numTimesHit(1)
      result.damageDealt(damageToDeal)
      result.processTakeDamageResult(defender, damageResult)
      result.merge(mrb)
    }
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

class TestOneHitKO extends PhysicalMove with OneHitKO {
  override val index = 999
  override val maxPP = 5
  override val accuracy = 1.0
}


trait SingleStrikeLoseHPOnMiss extends Move {
  def hpToLoseOnMiss: Int
  def typesMissAgainst: Set[Type]

  // TODO: Does the Pokemon or the substitute take the HP hit if you miss?
  abstract override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    // This logic was too ugly for an "if" clause
    def moveHits: Boolean = {
      if (typesMissAgainst.contains(defender.type1)) false
      else if (typesMissAgainst.contains(defender.type2)) false
      else if (!pb.statusManager.canBeHit(defender)) false
      else Random.nextDouble < chanceHit(attacker, defender, pb)
    }

    if (moveHits) {
      val result = pb.dc.calc(attacker, defender, this, pb)
      val damageResult = defender.takeDamage(result.damageDealt)

      // moveIndex, damageDealt, critHit, STAB, moveType, typeMult from calc
      result.processTakeDamageResult(defender, damageResult)
      result.merge(mrb)
      // hpGained, statusChange, selfKO irrelevant
      super.moveSpecificStuff(attacker, defender, pb, result)

    } else {
      // deal yourself damage, then record the result
      val missResult = new MoveResultBuilder().moveIndex(index).moveType(type1)

      val damageResult = attacker.takeDamage(hpToLoseOnMiss min attacker.currentHP())
      // DIFFERENT - all defaults are correct, except you need to check for selfKO
      damageResult match {
        case KO => { missResult.selfKO(true); assert(!(attacker.isAlive)) }
        case SUBKO => {}
        case ALIVE => {}
      }
      missResult.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, missResult)
    }
  }
}


trait DamageEqualsUserLevel extends Move {
  // SeismicToss and NightShade both deal damage equal to the attacker's level,
  // ignoring all STABs, type effectiveness, etc.
  abstract override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {
      val damageToDeal = attacker.level min defender.currentHP()
      val damageResult = defender.takeDamage(damageToDeal)

      // Build an MRB from scratch
      result.damageCalc(attacker.level)
      result.numTimesHit(1)
      result.damageDealt(damageToDeal)
      // no hpGained, critHit, STAB, typeMult, nvsa, vsa, stat changes
      result.processTakeDamageResult(defender, damageResult)
    }
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}



trait SuicideDamage extends Move {
  /*
   * This trait KOs its user and deals a good amount of damage to the defender.
   *
   * This functionality is used by Explosion and Selfdestruct in Gen 1.
   *
   * Incredibly, a bug in the game allows you lose 0 HP if you use one of these
   * moves to break a substitute. See for example
   * https://www.youtube.com/watch?v=lr05doU5oAQ
   */
  def defenderDefenseProp = 0.5

  abstract override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    // result contains correct info for a miss
    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)

    // If you hit, process the hit and update result
    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {
      val dcResult = pb.dc.calc(attacker, defender, this, pb)
      result.merge(dcResult)
      val damageResult = defender.takeDamage(result.damageDealt)
      result.processTakeDamageResult(defender, damageResult)
      // numTimesHit == 1, no hpGained, no statusAilments, no stat changes
    }

    // Faint unless you KOed a sub AND Glitch.suicideGlitchOn
    if (!(result.subKO && Glitch.suicideGlitchOn)) {
      result.selfKO(true)
      attacker.takeDamage(attacker.currentHP())
    }

    // Combine and pass along
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}


trait GainPropDamageDealt extends Move {
  /*
   * This trait captures the property of LeechLife, Absorb, MegaDrain, and
   * DreamEater, whereby up to 50% of the damage dealt by a single strike
   * attack is restored to the attacker. These moves should extend some Strike
   * on the right so that GainPropDamageDealt has access to mrb.damageDealt.
   *
   * This trait is never used on its own: you transfer HP to yourself
   * iff a Strike succeeded first.
   *
   * No HP is restored if the move BREAKS a substitute.
   *
   * SingleStrike has a member requiredStatusAilments: Set[StatusAilment].
   * This was inspired by DreamEater, and means that DreamEater can extend
   * GainPropDamageDealt without worrying about whether the defender is asleep:
   * SingleStrike will take care of that, and DreamEater just checking for
   * damage dealt is good enough.
   *
   * NOTE: These moves always restore health to the underlying Pokemon, even if
   * that Pokemon has a substitute active.
   */

  def propToRestore = 0.5

  abstract override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    // Start with a new MRB so that we don't mutate mrb
    val result = new MoveResultBuilder()
    result.merge(mrb)

    if (result.moveIndex == -1)
      throw new Exception("GainPropDamageDealt called without a former trait")

    // The SingleStrike should have filled in most of the MRB details already
    // We just worry about gaining HP here
    val shouldTransferHP = mrb.numTimesHit == 1 && !mrb.subKO
    if (shouldTransferHP) {
      val potentialGain = result.damageDealt match {
        case 1 => 1
        case n => n / 2
      }
      val actualGain =
        potentialGain min (attacker.maxHP - attacker.currentHP(true))
      attacker.gainHP(actualGain)
      result.hpGained(actualGain)
    }

    // Already merged, just pass things along
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

trait RegisterViolentStruggle extends Move {
  /*
   * This trait captures the behavior of Thrash and PetalDance in Gen 1
   * TODO: Fill this in
   */

  def vsType: ViolentStruggleType

  abstract override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

trait WaitThenAttack extends Move {
  /*
   * This trait captures the behavior of SkyAttack, SkullBash, SolarBeam, and
   * RazorWind in Gen1. Also Fly and Dig, though those two make most Moves
   * miss.
   * TODO: Fill this in. These are mostly registration moves, so find some way
   *       to sign them up to the correct data structure
   */

  abstract override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}


trait PartiallyTrapping extends Move {
  /*
   * This trait captures the behavior of Bind, Wrap, Clam, and FireSpin
   * in Gen1.
   *
   * TODO: Fill this in. These are mostly registration moves, so find some way
   *       to sign them up to the correct data structure
   */

  abstract override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    result.merge(mrb)
    // TODO: A partially trapping move, even if it misses, relieves the opponent
    // of HyperBeam recharge. So check to see if pb.weirdMoveMan.hasHBDelay(defender),
    // and if so, negate the delay
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}


trait RestoreHP extends Move {
  /*
   * This trait captures the behavior of Recover and Softboiled in Gen 1:
   * Up to 50% of the user's maxHP is restored.
   *
   * There's a bug that makes this fail in certain numeric cases; that
   * logic is encoded below and is controlled by the Glitch object.
   */

  def proportionToRestore = 0.5

  abstract override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)

    val currentHP = attacker.currentHP()
    val maxHP = attacker.maxHP
    if (currentHP > maxHP) {
      // weird case that I doubt you'll ever find yourself in
      attacker.toFullHealth()
    } else if (currentHP == maxHP) {
      // no recovering necessary
    } else if (Glitch.recoverGlitch && ((maxHP - currentHP) + 1) % 256 == 0) {
      // glitch - do nothing!
    } else {
      // let the healing begin
      val hpToHeal = (maxHP * proportionToRestore).toInt
      attacker.gainHP(hpToHeal)
      result.hpGained(hpToHeal)
    }

    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}
