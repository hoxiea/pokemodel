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
 * Ideally, something about stackable traits with a MoveResultBuilder that gets passed up and appended as it goes,
 * until it's converted to a MoveResult by either PhysicalMove, SpecialMove, or StatusMove
 */

// TODO: Any move that can cause a stat change to opponent needs to make sure that the opponent's stats can change via the battle's statManager
// TODO: moveSpecificStuff could return true/false if it succeeds/fails. This would let you, for example, print "It succeeded!" or "It failed!"
// TODO: Build some integrated way to figure out if a move hits that takes RNG, accuracy, attacker accuracy, evasion, statuses, Fly/Dig/Haze/etc. into account
// TODO: Build some way to capture the logic of inflicting a status ailment. Should check for: pre-existing condition, Haze, missed attack, etc.
// TODO: Redo this with traits like SingleStrike, MultiStrike, CauseStatusAilment, Recoil, etc. Then you can reuse the "cause status ailment" logic in all moves that do it instead of copying/pasting for Physical AND Special

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
    accuracy * (pb.statManager.getEffectiveAccuracy(attacker).toDouble / pb.statManager.getEffectiveEvasion(defender))
  }


  def startUsingMove(attacker: Pokemon, defender: Pokemon, pb: Battle) {
    // Function called before move-specific stuff happens
    if (VERBOSE) println(s"${attacker.name} used $this on ${defender.name}")
  }

  // This is what each specific move is ultimately responsible for providing, along with basic information
  def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle, mrb: MoveResultBuilder): MoveResult

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
 * Physical, Special, and Status. These tack on a moveType value, which
 * the Battle's DamageCalculator uses to determine the correct stats to use.
 * They also give default values to the abstract members of Move, so that
 * the stackable traits pattern will fly
 */
class PhysicalMove extends Move {
  override val index = 0
  override val type1 = Normal
  override val power = 0
  override val maxPP = 0
  override val moveType = PHYSICALMOVE
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle, mrb: MoveResultBuilder) = mrb.toMoveResult
}

class SpecialMove extends Move {
  override val index = 0
  override val type1 = Normal
  override val power = 0
  override val maxPP = 0
  override val moveType = SPECIALMOVE
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle, mrb: MoveResultBuilder) = mrb.toMoveResult
}

class StatusMove extends Move {
  override val index = 0
  override val type1 = Normal
  override val power = 0
  override val maxPP = 0
  override val moveType = STATUSMOVE
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle, mrb: MoveResultBuilder) = mrb.toMoveResult
}


/*
 * I originally just subclassed these three guys to specialize them further:
 * PhysicalSingleStrike, PhysicalSingleStrikeStatusChange, SpecialSingleStrikeStatChange,
 * stuff like that. But I found myself duplicating efforts, and realized that mixing in
 * traits was the way to go.
 */

// These allow you to change the type of a move on the fly, which is insanely useful
// for testing purposes. For example,
// val m = new TestPhysicalSingleStrike with Electric/Psychic/Whatever
trait Normal extends Move { override val type1 = Normal }
trait Fighting extends Move { override val type1 = Fighting }
trait Flying extends Move { override val type1 = Flying }
trait Poison extends Move { override val type1 = Poison }
trait Ground extends Move { override val type1 = Ground }
trait Rock extends Move { override val type1 = Rock }
trait Bug extends Move { override val type1 = Bug }
trait Ghost extends Move { override val type1 = Ghost }
trait Fire extends Move { override val type1 = Fire }
trait Water extends Move { override val type1 = Water }
trait Grass extends Move { override val type1 = Grass }
trait Electric extends Move { override val type1 = Electric }
trait Psychic extends Move { override val type1 = Psychic }
trait Ice extends Move { override val type1 = Ice }
trait Dragon extends Move { override val type1 = Dragon }

// Next, we capture commonalities in terms of the different things that Moves do
trait SingleStrike extends Move {
  abstract override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle, mrb: MoveResultBuilder = new MoveResultBuilder()) = {
    super.moveSpecificStuff(attacker, defender, pb, mrb)
    println("Calling SingleStrike's moveSpecificStuff")
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val result = pb.dc.calc(attacker, defender, this, pb)
      defender.takeDamage(result.damageDealt)
      result.KO(defender.isAlive)
      result.toMoveResult
    } else new MoveResultBuilder().toMoveResult  // default values are actually correct here
  }
}

//trait StatusChange extends Move {
//  // Cause some kind of StatusAilment to the opponent, non-volatile or volatile,
//  // with a probability that depends on the move
//  def statusAilmentToCause   : StatusAilment
//  def chanceOfCausingAilment : Double
//  
//  def statusAilmentCaused : Boolean = Random.nextDouble < chanceOfCausingAilment
//
//  abstract override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle, mrb: MoveResultBuilder = new MoveResultBuilder()) = {
//    super.moveSpecificStuff(attacker, defender, pb, mrb)
//    if (statusAilmentCaused) { 
//      statusAilmentToCause match {
//        case (_ : NonVolatileStatusAilment) => {
//          val changeWorked = pb.statusManager.tryToChangeStatusAilment(defender, statusAilmentToCause)
//          if (changeWorked) new MoveResultBuilder().statusChange(statusAilmentToCause).toMoveResult
//          else new MoveResultBuilder().toMoveResult
//        }
//        case (_ : CONFUSION) => {
//          val changeWorked = pb.statusManager.tryToCauseConfusion(defender)
//          if (changeWorked) new MoveResultBuilder().statusChange(new CONFUSION).toMoveResult
//          else new MoveResultBuilder().toMoveResult
//        }
//        case (_ : FLINCH) => pb.statusManager.causeToFlinch(defender); false
//        case (_ : PARTIALLYTRAPPED) => pb.statusManager.tryToPartiallyTrap(defender); false
//        case (_ : SEEDED) => pb.statusManager.tryToSeed(defender); false
//        case _ => false
//      }
//    } else false
//  }
//}

//trait Recoil extends Move {
//  // Take damage equal to some proportion of the damage dealt to the opponent, usually 25%
//  def recoilProportion: Double
//  abstract override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle, mrb: MoveResultBuilder = new MoveResultBuilder()) = {
//    super.moveSpecificStuff(attacker, defender, pb, mrb)
//    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
//      val result = pb.dc.calc(attacker, defender, this, pb)
//      defender.takeDamage(result.damageDealt)
//      val recoilDamage = (result.damageDealt * recoilProportion).toInt
//      attacker.takeDamage(recoilDamage)
//      if (VERBOSE) println(s"${attacker.name} took $recoilDamage recoil damage")
//      result.KO(defender.isAlive)
//      result.selfKO(attacker.isAlive)
//      result.toMoveResult
//    } else false
//  }
//}

trait ConstantDamage extends Move {
  def damageAmount: Int
}


class TestPhysicalSingleStrike extends PhysicalMove with SingleStrike with Normal {
  override val index = 999
  override val power = 40
  override val maxPP = 20
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

class Thunder extends SpecialMove with SingleStrike {
//  TODO: add StatusChange
  override val index = 87
  override val type1 = Electric
  override val power = 110
  override val maxPP = 10
  override val accuracy = 0.7

//  override def statusAilmentToCause = new PAR
//  override def chanceOfCausingAilment = 0.1
}

class Struggle extends PhysicalMove with SingleStrike {
  // TODO: Add Recoil! 50%
  override val index = 165
  override val type1 = Normal
  override val power = 50
  override val maxPP = 1
//  override val recoilProportion = 0.5   // different from others!

  override def finishUsingMove(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // Don't deduct a PP! Just log it
    pb.moveManager.updateLastMoveIndex(attacker, index)
  }
}


