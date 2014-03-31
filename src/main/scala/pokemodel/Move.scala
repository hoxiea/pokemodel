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

  // Even moves with 100% accuracy might miss because of accuracy/evasion adjustments in battle
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
trait CritHit extends Move  { override val critHitRate = HIGH }

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
      mrb.merge(result)
    }

    // Pass these changes along to the next moveSpecificStuff
    super.moveSpecificStuff(attacker, defender, pb, mrb)
  }
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

    if (Random.nextDouble < chanceHit(attacker, defender, pb) && pb.statusManager.canBeHit(defender)) {
      // Bypass DamageCalculator, so we have to do this stuff ourselves
      val damageToDeal = damageAmount min defender.currentHP
      defender.takeDamage(damageToDeal)
      mrb.damageDealt(damageToDeal)
      mrb.KO(!defender.isAlive)
      mrb.selfKO(!attacker.isAlive)
    }
    super.moveSpecificStuff(attacker, defender, pb, mrb)
  }
}

trait Recoil extends Move {
  // Take damage equal to some proportion of the damage dealt to the opponent, usually 25%
  // Because of the fact that the damage must be dealt first, Recoil should only be mixed
  // in TO THE LEFT of SingleStrike. I check for this below
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
    if (mrb.damageDealt == 0)
      println("""|Recoil lacks damage in moveSpecificStuff - attack missed,
                 |unaffective movetype, or Recoil mixed in wrong""".stripMargin)
    val damageToTake =
      (mrb.damageDealt * recoilProportion).toInt min attacker.currentHP
    attacker.takeDamage(damageToTake)
    mrb.selfKO(!attacker.isAlive)
    super.moveSpecificStuff(attacker, defender, pb, mrb)
  }
}


class TestPhysicalSingleStrike extends PhysicalMove with SingleStrike {
  override val index = 999
  override val type1 = Normal
  override val power = 40
  override val maxPP = 20
}

/* Using just these few pieces, we can implement a surprising number of moves */
class Pound extends PhysicalMove with SingleStrike {
  override val index = 1
  override val type1 = Normal
  override val power = 40
  override val maxPP = 35
}

class Tackle extends PhysicalMove with SingleStrike {
  override val index = 33
  override val type1 = Normal
  override val power = 50
  override val maxPP = 35
}

class KarateChop extends PhysicalMove with SingleStrike {
  override val index = 2
  override val type1 = Normal    // Fighting in later generations
  override val power = 50
  override val maxPP = 25
  override val critHitRate = HIGH
}

class DragonRage extends SpecialMove with ConstantDamage {
  override val index = 82
  override val type1 = Dragon
  override val maxPP = 10
  override def damageAmount = 40
}

class Thunder extends SpecialMove with SingleStrike with StatusChange {
  override val index = 87
  override val type1 = Electric
  override val power = 110
  override val maxPP = 10
  override val accuracy = 0.7

  override def statusAilmentToCause = new PAR
  override def chanceOfCausingAilment = 0.1
}

class Struggle extends PhysicalMove with Recoil with SingleStrike {
  override val index = 165
  override val type1 = Normal
  override val power = 50
  override val maxPP = 999
  override val recoilProportion = 0.5   // different from others!

  override def finishUsingMove(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // Don't deduct a PP! Just log it
    pb.moveManager.updateLastMoveIndex(attacker, index)
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
    if (statusAilmentCaused) {
      statusAilmentToCause match {
        case (_ : NonVolatileStatusAilment) => {
          if (pb.statusManager.changeMajorStatusAilment(defender, statusAilmentToCause)) {
            mrb.statusChange(statusAilmentToCause)
          }
        }
        case (_ : CONFUSION) => {
          if (pb.statusManager.tryToCauseConfusion(defender)) {
            mrb.statusChange(statusAilmentToCause)
          }
        }
        case (_ : FLINCH) => {
          if (pb.statusManager.causeToFlinch(defender)) {
            mrb.statusChange(statusAilmentToCause)
          }
        }
        case (_ : PARTIALLYTRAPPED) => {
          if (pb.statusManager.tryToPartiallyTrap(defender)) {
            mrb.statusChange(statusAilmentToCause)
          }
        }
        case (_ : SEEDED) => {
          if (pb.statusManager.tryToSeed(defender)) {
            mrb.statusChange(statusAilmentToCause)
          }
        }
      }
    }
    super.moveSpecificStuff(attacker, defender, pb, mrb)
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

//class TestPhysicalMultiStrike extends PhysicalMove with MultiStrike with Normal {
//  override val index = 999
//  override val power = 25
//  override val maxPP = 20
//}

//trait MultiStrike extends Move {
//  // Hit the opponent 2, 3, 4, or 5 times with the Gen 1 probabilities (0.375, 0.375, 0.125, 0.125)
//  // TODO: Ends if substitute breaks
//  // TODO: Make sure that Bide and Counter only acknowledge the last attack in this sequence
//  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle, mrb: MoveResultBuilder = new MoveResultBuilder()) = {
//    super.moveSpecificStuff(attacker, defender, pb, mrb)
//    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
//      val r = Random.nextDouble
//      val numStrikes = if (r < 0.375) 2
//                       else if (r < (0.375 + 0.375)) 3
//                       else if (r < (0.375 + 0.375 + 0.125)) 4
//                       else 5
//
//      // In later generations, each hit was considered separately and could be critical/not
//      // In Gen 1, damage was calculated once and then used for each blow
//      val result = pb.dc.calc(attacker, defender, this, pb)
//
//      // There's no harm in doing damage once the enemy is dead, since takeDamage stops at 0 HP
//      for (_ <- 1 to numStrikes) {
//        defender.takeDamage(result.damageDealt)
//      }
//
//      if (VERBOSE) println(s"$this hit ${defender.name} $numStrikes times!")
//      result.KO(defender.isAlive)
//      result.toMoveResult
//    } else false
//  }
//}

