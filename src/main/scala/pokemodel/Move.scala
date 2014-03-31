package pokemodel

import scala.collection.mutable
import Type._
import MoveType._
import BattleStat._
import CritHitType._
import scala.util.Random
import Battle.{verbose=>VERBOSE}

/*
 * TODO: Overview of the final version of the Move design goes here
 * Ideally, something about stackable traits with a MoveResultBuilder that gets
 * passed up and appended as it goes, until it's converted to a MoveResult by
 * either PhysicalMove, SpecialMove, or StatusMove
 */

// TODO: Any move that can cause a stat change to opponent needs to make sure that the opponent's stats can change via the battle's statManager
// TODO: moveSpecificStuff could return true/false if it succeeds/fails. This would let you, for example, print "It succeeded!" or "It failed!"
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
  abstract override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {
      val result = pb.dc.calc(attacker, defender, this, pb)
      defender.takeDamage(result.damageDealt)
      result.KO(!defender.isAlive)
      result.numTimesHit(1)  // singleStrike
      result.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, result)
    } else {
      // Missed, so nothing in MRB changes... pass along what you got
      super.moveSpecificStuff(attacker, defender, pb, mrb)
    }
  }
}

class TestPhysicalSingleStrike extends PhysicalMove with SingleStrike {
  override val index = 999
  override val type1 = Normal
  override val power = 40
  override val maxPP = 20
}


trait ConstantDamage extends Move {
  def damageAmount: Int
  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    // println("Calling ConstantDamage's moveSpecificStuff")
    /*
     * A ConstantDamage attack can do the following things:
     * - Deal Damage
     * - KO
     * - selfKO?
     */

    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {
      val damageToDeal = damageAmount min defender.currentHP
      defender.takeDamage(damageToDeal)

      // Bypass DamageCalculator, so we have to do this stuff ourselves.
      // Don't mutate mrb!
      val result = new MoveResultBuilder()
      result.damageDealt(damageToDeal)
      result.numTimesHit(1)
      // no crithits, STAB, moveType, typeMult, or statusChange for
      // ConstantDamage moves
      result.KO(!defender.isAlive)
      result.selfKO(!attacker.isAlive)
      super.moveSpecificStuff(attacker, defender, pb, result)
    } else {
      super.moveSpecificStuff(attacker, defender, pb, mrb)
    }
  }
}


trait Recoil extends Move {
  // Take damage equal to some proportion of the damage dealt to the opponent,
  // usually 25%. Because of the fact that the damage must be dealt before
  // recoil can figure out how much damage to deal, Recoil should only be mixed
  // in TO THE LEFT of SingleStrike
  def recoilProportion: Double

  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    // println("Calling Recoil's moveSpecificStuff")
    /*
     * A Recoil just hurts the user itself, so we have to worry about
     * - selfKO
     */
    // if (mrb.damageDealt == 0)
    //   println("""|Recoil lacks damage in moveSpecificStuff - attack missed,
    //              |unaffective movetype, or Recoil mixed in wrong""".stripMargin)
    val damageToTake =
      (mrb.damageDealt * recoilProportion).toInt min attacker.currentHP
    attacker.takeDamage(damageToTake)
    val result = new MoveResultBuilder()
    result.selfKO(!attacker.isAlive)
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}


trait StatusChange extends Move {
  // Cause some kind of StatusAilment to the opponent, non-volatile or
  // volatile, with a probability that depends on the move
  def statusAilmentToCause   : StatusAilment
  def chanceOfCausingAilment : Double
  def statusAilmentCaused: Boolean = Random.nextDouble < chanceOfCausingAilment

  abstract override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {
    val result = new MoveResultBuilder
    if (statusAilmentCaused) {
      statusAilmentToCause match {
        case (_ : NonVolatileStatusAilment) => {
          if (pb.statusManager.changeMajorStatusAilment(defender, statusAilmentToCause)) {
            result.statusChange(statusAilmentToCause)
          }
        }
        case (_ : CONFUSION) => {
          if (pb.statusManager.tryToCauseConfusion(defender)) {
            result.statusChange(statusAilmentToCause)
          }
        }
        case (_ : FLINCH) => {
          if (pb.statusManager.causeToFlinch(defender)) {
            result.statusChange(statusAilmentToCause)
          }
        }
        case (_ : PARTIALLYTRAPPED) => {
          if (pb.statusManager.tryToPartiallyTrap(defender)) {
            result.statusChange(statusAilmentToCause)
          }
        }
        case (_ : SEEDED) => {
          if (pb.statusManager.tryToSeed(defender)) {
            result.statusChange(statusAilmentToCause)
          }
        }
      }
    }
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

class TestBurner extends SpecialMove with StatusChange {
  override val index = 999
  override val type1 = Fire  // shouldn't be used
  override val power = 40    // shouldn't be used
  override val maxPP = 10
  override val accuracy = 1.0

  override def statusAilmentToCause = new BRN
  override def chanceOfCausingAilment = 1.0
}

class TestAsleep extends SpecialMove with StatusChange {
  override val index = 999
  override val type1 = Normal  // shouldn't be used
  override val power = 40      // shouldn't be used
  override val maxPP = 10
  override val accuracy = 1.0  // always hit, for test purposes

  override def statusAilmentToCause = new SLP
  override def chanceOfCausingAilment = 1.0  // always cause, for test purposes
}



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

    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
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

      /*
       * At this point, we have everything we need to figure out how many times
       * to strike and how much damage to deal on each strike. You might think
       * that this would be numStrikes and result.damageDealt, but you'd be wrong.
       * Not only does Pokemon.takeDamage NOT absorb excess damage anymore, but
       * moves like Bide and Counter only use the last strike in the sequence,
       * so we actually have to figure out the damage sequence that either deals
       * result.damageDealt damage all the way through, or stops short upon
       * killing opponent, breaking substitute, etc. Then we'll update result,
       * merge it into mrb, and be on our way.
       */

      def damageSeqCalc(
        numStrikesLeft: Int,
        defenderHPLeft: Int,
        soFar: List[Int]): List[Int] = {
        if (numStrikesLeft == 0 || defenderHPLeft <= 0) soFar.reverse
        else {
          val damageToDeal = result.damageDealt min defenderHPLeft
          damageSeqCalc(numStrikesLeft - 1,
                        defenderHPLeft - damageToDeal,
                        damageToDeal :: soFar)
        }
      }

      val damageSeq = damageSeqCalc(numStrikes, defender.currentHP, List[Int]())
      assert(damageSeq.last > 0, "damageSeqCalc fail")

      // Actually deal the damage in damageSeq
      damageSeq.map(n => defender.takeDamage(n))

      // Update result
      result.numTimesHit(damageSeq.length)
      result.damageDealt(damageSeq.last)
      result.KO(!defender.isAlive)
      result.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, result)
    } else {
      // Attack missed, so just pass along what you received
      super.moveSpecificStuff(attacker, defender, pb, mrb)
    }
  }
}

class TestMultiStrike extends PhysicalMove with MultiStrike {
  override val index = 999
  override val power = 40
  override val maxPP = 20
  override val accuracy = 1.0
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
  super.moveSpecificStuff(attacker, defender, pb, mrb)
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
  super.moveSpecificStuff(attacker, defender, pb, mrb)
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

    val result = new MoveResultBuilder()
    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {
      val damageToDeal = defender.currentHP
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

