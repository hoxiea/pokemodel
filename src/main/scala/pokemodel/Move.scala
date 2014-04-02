package pokemodel

import scala.collection.mutable
import Type._
import MoveType._
import BattleStat._
import CritHitType._
import TakeDamageResult._
import scala.util.Random
import Battle.{verbose=>VERBOSE}

/*
 * TODO: Overview of the final version of the Move design goes here
 * Ideally, something about stackable traits with a MoveResultBuilder that gets
 * passed up and appended as it goes, until it's converted to a MoveResult by
 * either PhysicalMove, SpecialMove, or StatusMove
 */

// TODO: Any move that can cause a stat change to opponent needs to make sure that the opponent's stats can change via the battle's statManager
// TODO: Build some integrated way to figure out if a move hits that takes RNG, accuracy, attacker accuracy, evasion, statuses, Fly/Dig/Haze/etc. into account

/*
 * All Moves are a subclass of type Move, which defines what it means to be a move.
 * It captures the basic but necessary information about a move, and makes sure that
 * all Moves do the required set-up and clean-up by declaring #use to be final.
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

  def startUsingMove(attacker: Pokemon, defender: Pokemon, pb: Battle) {
    // Function called before move-specific stuff happens
  }

  // This is what each specific move is ultimately responsible for filling in
  // along with index and type and that stuff
  def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder): MoveResult

  def finishUsingMove(attacker: Pokemon, defender: Pokemon, pb: Battle) {
    // Function called after move-specific stuff happens
    pb.moveManager.updateLastMoveIndex(attacker, index)
  }

  final def use(attacker: Pokemon, defender: Pokemon, pb: Battle): MoveResult = {
    startUsingMove(attacker, defender, pb)
    val result = moveSpecificStuff(attacker, defender, pb, mrb)
    finishUsingMove(attacker, defender, pb)
    result
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
    mrb: MoveResultBuilder) = mrb.toMoveResult
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
    mrb: MoveResultBuilder) = mrb.toMoveResult
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
    mrb: MoveResultBuilder) = mrb.toMoveResult
}


/*
 * I originally just subclassed these three guys to specialize them further:
 * PhysicalSingleStrike, PhysicalSingleStrikeStatusChange,
 * SpecialSingleStrikeStatChange, stuff like that. But I found myself
 * duplicating efforts: for example, there are PhysicalSingleStrike moves and
 * there are SpecialSingleStrike moves, and the logic for both was basically
 * the same, just the stats involved changed.
 *
 * I then realized that stacking traits would be the perfect way to capture the
 * different types of moves just once.
 */

// These allow you to change the type of a move on the fly. For example,
// val m1 = new TestPhysicalSingleStrike with Electric
// val m2 = new TestPhysicalSingleStrike with Psychic
// Very useful for testing effectivenesses, etc.
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
trait Psychic extends Move  { override val type1 = Psychic }
trait Ice extends Move      { override val type1 = Ice }
trait Dragon extends Move   { override val type1 = Dragon }

// Likewise, handy ways to change the base power on the fly
trait Power40 extends Move  { override val power = 40 }
trait Power80 extends Move  { override val power = 80 }
trait Power120 extends Move { override val power = 120 }

// Quick way to get more critical hits, for testing purposes
trait NeverCritHit extends Move  { override val critHitRate = NEVER }
trait HighCritHit extends Move   { override val critHitRate = HIGH }
trait AlwaysCritHit extends Move { override val critHitRate = ALWAYS }

// High priority, for testing purposes
trait HighPriority extends Move  { override val priority = 1 }


/* Next, capture common behaviors of Moves - damage, stats, status, etc. */
trait SingleStrike extends Move {
  // A basic damage-dealing experience
  // Any move that strikes once and uses DamageCalculator to figure out the
  // appropriate damage will extend SingleStrike
  abstract override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {
      val result = pb.dc.calc(attacker, defender, this, pb)
      val damageResult = defender.takeDamage(result.damageDealt)

      // Update result values
      // numTimesHit == 1, no hpGained, no statusAilments, no stat changes
      damageResult match {
        case KO => { result.KO(true); assert(!(defender.isAlive)) }
        case SUBKO => result.subKO(true)
        case ALIVE => {}
      }

      // Combine anything passed in from traits further to the right
      result.merge(mrb)

      // Pass at all along to the next trait/class
      super.moveSpecificStuff(attacker, defender, pb, result)

    } else {
      // Pass along what you got + moveIndex
      val missResult = new MoveResultBuilder().moveIndex(index)
      missResult.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, missResult)
    }
  }
}

class TestPhysicalSingleStrike extends PhysicalMove with SingleStrike {
  override val index = 999
  override val power = 40
  override val maxPP = 20
}


trait ConstantDamage extends Move {
  // Deal a given amount of damage, ignoring type effectiveness, STAB, etc.
  def damageAmount: Int

  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    // In this case, we skip DamageCalculator, so we build a MRB from scratch
    val result = new MoveResultBuilder().moveIndex(index)

    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {
      val damageToDeal = damageAmount min defender.currentHP()
      val hitResult = defender.takeDamage(damageToDeal)

      result.damageCalc(damageAmount)
      result.numTimesHit(1)
      result.damageDealt(damageToDeal)

      // no crithits, STAB, moveType, typeMult, or statusChange for
      // ConstantDamage moves
      hitResult match {
        case KO => { result.KO(true); assert(!(defender.isAlive)) }
        case SUBKO => result.subKO(true)
        case ALIVE => {}
      }
      result.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, result)
    } else {
      // Pass along what you got + moveIndex
      result.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, result)
    }
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
    val result = new MoveResultBuilder().moveIndex(index)
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


trait NonVolatileStatusChange extends Move {
  // Cause some kind of StatusAilment to the opponent, non-volatile or
  // volatile, with a probability that depends on the move
  def statusAilmentToCause   : NonVolatileStatusAilment
  def chanceOfCausingAilment : Double
  def statusAilmentCaused: Boolean = Random.nextDouble < chanceOfCausingAilment

  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val result = new MoveResultBuilder().moveIndex(index)
    if (statusAilmentCaused) {
      if (pb.statusManager.changeMajorStatusAilment(defender, statusAilmentToCause)) {
        result.nvsa(statusAilmentToCause)
      }
    }
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}


class TestBurner extends SpecialMove with NonVolatileStatusChange {
  override val index = 999
  override val type1 = Fire  // shouldn't be used
  override val power = 40    // shouldn't be used
  override val maxPP = 10
  override val accuracy = 1.0

  override def statusAilmentToCause = new BRN
  override def chanceOfCausingAilment = 1.0
}

class TestAsleep extends SpecialMove with NonVolatileStatusChange {
  override val index = 999
  override val type1 = Normal  // shouldn't be used
  override val power = 40      // shouldn't be used
  override val maxPP = 10
  override val accuracy = 1.0  // always hit, for test purposes

  override def statusAilmentToCause = new SLP
  override def chanceOfCausingAilment = 1.0  // always cause, for test purposes
}


trait VolatileStatusChange extends Move {
  // Cause some kind of StatusAilment to the opponent, non-volatile or
  // volatile, with a probability that depends on the move
  def statusAilmentToCause   : VolatileStatusAilment
  def chanceOfCausingAilment : Double
  def statusAilmentCaused: Boolean = Random.nextDouble < chanceOfCausingAilment

  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val result = new MoveResultBuilder().moveIndex(index)
    if (statusAilmentCaused) {
      statusAilmentToCause match {
        case (_ : CONFUSION) => {
          if (pb.statusManager.tryToCauseConfusion(defender)) {
            result.vsa(statusAilmentToCause)
          }
        }
        case (_ : FLINCH) => {
          if (pb.statusManager.causeToFlinch(defender)) {
            result.vsa(statusAilmentToCause)
          }
        }
        case (_ : PARTIALLYTRAPPED) => {
          if (pb.statusManager.tryToPartiallyTrap(defender)) {
            result.vsa(statusAilmentToCause)
          }
        }
        case (_ : SEEDED) => {
          if (pb.statusManager.tryToSeed(defender)) {
            result.vsa(statusAilmentToCause)
          }
        }
      }
    }
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

// TODO: Test VolatileStatusChange


trait MultiStrike extends Move {
  // Hit the opponent 2, 3, 4, or 5 times with the Gen 1 probabilities (0.375,
  // 0.375, 0.125, 0.125)
  // TODO: Ends if substitute breaks
  // TODO: Ensure that Bide & Counter only see the last attack in this sequence
  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {
      val r = Random.nextDouble
      val numStrikes = if (r < 0.375) 2
                       else if (r < (0.375 + 0.375)) 3
                       else if (r < (0.375 + 0.375 + 0.125)) 4
                       else 5

      /*
       * In later generations, each hit was considered separately and could be
       * critical/not. In Gen 1, damage was calculated once and then used for
       * each blow
       */
      val result = pb.dc.calc(attacker, defender, this, pb)
      val damageEachStrike = result.damageDealt

      // Figure out what sequence of damages we should actually deal
      val damageSeq = Utils.damageSeqCalc(numStrikes, damageEachStrike, defender.currentHP())
      assert(damageSeq.last > 0, "damageSeqCalc fail")

      // Actually deal the damage in damageSeq
      damageSeq.map(n => defender.takeDamage(n))

      // Update result - index filled in by calc
      result.numTimesHit(damageSeq.length)
      result.damageDealt(damageSeq.last)
      result.KO(!defender.isAlive)
      result.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, result)
    } else {
      // Pass along what you got + moveIndex
      val missResult = new MoveResultBuilder().moveIndex(index)
      missResult.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, missResult)
    }
  }
}

class TestMultiStrike extends PhysicalMove with MultiStrike {
  override val index = 999
  override val power = 40
  override val maxPP = 20
  override val accuracy = 1.0
}


trait DoubleStrike extends Move {
  // Very similar to MultiStrike, except it always hits exactly twice
  // TODO: Ends if substitute breaks
  // TODO: Ensure that Bide & Counter only see the last attack in this sequence
  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {
      val numStrikes = 2

      /*
       * In later generations, each hit was considered separately and could be
       * critical/not. In Gen 1, damage was calculated once and then used for
       * each blow
       */
      val result = pb.dc.calc(attacker, defender, this, pb)
      val damageEachStrike = result.damageDealt

      /*
       * Same deal as MultiStrike - at this point, we should figure out how much
       * damage we're going to deal on each hit, taking fainting into account
       */
      val damageSeq = Utils.damageSeqCalc(numStrikes, damageEachStrike, defender.currentHP())
      assert(damageSeq.last > 0, "damageSeqCalc fail")

      // Actually deal the damage in damageSeq
      damageSeq.map(n => defender.takeDamage(n))

      // Update result - moveIndex from calc
      result.numTimesHit(2)
      result.damageDealt(damageSeq.last)
      result.KO(!defender.isAlive)
      result.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, result)
    } else {
      // Pass along what you got + moveIndex
      val missResult = new MoveResultBuilder().moveIndex(index)
      missResult.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, missResult)
    }
  }
}


trait SelfStatChange extends Move {
  // Change one of your own Battle Stats
  def statToChange: BattleStat
  def amountToChangeBy: Int

  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    if (pb.statManager.canChangeOwnStats(attacker, pb)) {
      statToChange match {
        case ATTACK   => pb.statManager.changeAttackStage(attacker, amountToChangeBy)
        case DEFENSE  => pb.statManager.changeDefenseStage(attacker, amountToChangeBy)
        case SPEED    => pb.statManager.changeSpeedStage(attacker, amountToChangeBy)
        case SPECIAL  => pb.statManager.changeSpecialStage(attacker, amountToChangeBy)
        case ACCURACY => pb.statManager.changeAccuracyStage(attacker, amountToChangeBy)
        case EVASION  => pb.statManager.changeEvasionStage(attacker, amountToChangeBy)
      }
    }

    // Pass along what you got + moveIndex
    val result = new MoveResultBuilder().moveIndex(index)
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

class TestIncreaseSelfAttackStat extends StatusMove with SelfStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = ATTACK
  def amountToChangeBy = 3
}


class TestIncreaseSelfDefenseStat extends StatusMove with SelfStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = DEFENSE
  def amountToChangeBy = 3
}


trait EnemyStatChange extends Move {
  // Change your opponent's battle stats
  def statToChange: BattleStat
  def amountToChangeBy: Int
  def chanceOfStatChange: Double

  def statChangeHits: Boolean = Random.nextDouble < chanceOfStatChange

  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    if (statChangeHits &&
        pb.statManager.canChangeDefenderStats(attacker, defender, pb)) {
      statToChange match {
        case ATTACK   => pb.statManager.changeAttackStage(defender, amountToChangeBy)
        case DEFENSE  => pb.statManager.changeDefenseStage(defender, amountToChangeBy)
        case SPEED    => pb.statManager.changeSpeedStage(defender, amountToChangeBy)
        case SPECIAL  => pb.statManager.changeSpecialStage(defender, amountToChangeBy)
        case ACCURACY => pb.statManager.changeAccuracyStage(defender, amountToChangeBy)
        case EVASION  => pb.statManager.changeEvasionStage(defender, amountToChangeBy)
      }
    }

    // Pass along what you got + moveIndex
    val result = new MoveResultBuilder().moveIndex(index)
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

class TestDecreaseEnemyDefense extends StatusMove with EnemyStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = DEFENSE
  def amountToChangeBy = -3
  def chanceOfStatChange = 1.0
}


class TestDecreaseEnemyAttack extends StatusMove with EnemyStatChange {
  override val index = 999
  override val maxPP = 20
  def statToChange = ATTACK
  def amountToChangeBy = -3
  def chanceOfStatChange = 1.0
}


trait OneHitKO extends Move {
  // TODO: If there's a substitute and you hit, break the substitute
  abstract override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val result = new MoveResultBuilder().moveIndex(index)
    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {
      val damageToDeal = defender.currentHP()
      defender.takeDamage(damageToDeal)

      result.damageDealt(damageToDeal)
      result.KO(!defender.isAlive)
      result.moveType(type1)
      result.numTimesHit(1)
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

  // TODO: If there's a substitute and you hit, break the substitute
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
      defender.takeDamage(result.damageDealt min defender.currentHP())

      result.numTimesHit(1)
      // moveIndex, damageDealt, critHit, STAB, moveType, typeMult from calc
      result.KO(!defender.isAlive)
      result.moveType(type1)
      result.merge(mrb)
      // hpGained, statusChange, selfKO irrelevant
      super.moveSpecificStuff(attacker, defender, pb, result)

    } else {
      // deal yourself damage, then record the result
      attacker.takeDamage(hpToLoseOnMiss min attacker.currentHP())
      val missResult = new MoveResultBuilder().moveIndex(index)
      // all defaults are correct, except you need to check for selfKO
      missResult.selfKO(!attacker.isAlive)
      missResult.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, missResult)
    }
  }
}

