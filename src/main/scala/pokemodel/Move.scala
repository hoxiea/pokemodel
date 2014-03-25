package pokemodel

import scala.collection.mutable
import Type._
import MoveType._
import BattleStat._
import StatusAilment._
import CritHitType._
import scala.util.Random
import Battle.{verbose=>VERBOSE}

/* In the game, each Move is stored exactly once in memory, and the Pokemon Data Structure keeps
 * track of which Moves the Pokemon knows and how many PP are left for each Move the Pokemon has.
 *
 * I took a more object-oriented approach and modeled Moves as objects that
 * maintain their own statistics and state, and know how to do things like use
 * themselves against another Pokemon.
 */

// TODO: Any move that can cause a stat change to opponent needs to make sure opponent doesn't have Mist cast
// TODO: moveSpecificStuff could return true/false if it succeeds/fails. This would let you, for example, print "It succeeded!" or "It failed!"
// TODO: Build some integrated way to figure out if a move hits that takes RNG, accuracy, attacker accuracy, evasion, statuses, Fly/Dig/Haze/etc. into account
// TODO: Build some way to capture the logic of inflicting a status ailment. Should check for: pre-existing condition, Haze, missed attack, etc.

abstract class Move {
  /* ABSTRACT STUFF */
  val index : Int                // in 1 .. 165
  val type1 : Type.Value
  val moveType : MoveType.Value
  val power : Int

  var maxPP : Int
  var currentPP : Int

  def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle)

  /* IMPEMENTED STUFF */
  val critHitRate = LOW   // True for 99% of moves, outliers can override
  val priority = 0        // True for 99% of moves, outliers can override
  val accuracy = 1.0      // in [0.0, 1.0], true for ~60% of moves, others can override
  def restorePP(amount : Int) = { currentPP = intWrapper(maxPP) min (currentPP + amount) }
  def restorePP() = { currentPP = maxPP }

  def startUsingMove(attacker: Pokemon, defender: Pokemon, pb: Battle) : Unit = {
    assert(currentPP > 0, s"tried to use $this with 0 PP")
    if (VERBOSE) println(s"${attacker.name} used $this on ${defender.name}")
  }

  def finishUsingMove(attacker: Pokemon, defender: Pokemon, pb: Battle) : Unit = {
    currentPP -= 1
    pb.moveManager.updateLastMoveIndex(attacker, index)
  }

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle): Unit = {
    startUsingMove(attacker, defender, pb)
    moveSpecificStuff(attacker, defender, pb)
    finishUsingMove(attacker, defender, pb)
  }

  // Even moves with 100% accuracy might miss because of accuracy/evasion adjustments in battle
  def chanceHit(attacker : Pokemon, defender : Pokemon, pb : Battle): Double = {
    accuracy * (pb.statManager.getEffectiveAccuracy(attacker).toDouble / pb.statManager.getEffectiveEvasion(defender))
  }

  override def toString() = {
    val moveName = this.getClass().getName()
    val prefix = "pokemodel."
    if (moveName.startsWith(prefix)) moveName.substring(prefix.length)
    else moveName
  }
}

trait PhysicalMove extends Move {
  // Physical Moves use Attack and Defense as the relevant stats
  def getAttackStat(attacker: Pokemon, b : Battle)  = b.statManager.getEffectiveAttack(attacker)
  def getDefenseStat(defender: Pokemon, b : Battle) = b.statManager.getEffectiveDefense(defender)
  override val moveType = PHYSICALMOVE
}

trait SpecialMove extends Move {
  // Special Moves use Special as the relevant stat for both offense and defense in Gen 1
  def getAttackStat(attacker: Pokemon, b : Battle)  = b.statManager.getEffectiveSpecial(attacker)
  def getDefenseStat(defender: Pokemon, b : Battle)  = b.statManager.getEffectiveSpecial(defender)
  override val moveType = SPECIALMOVE
}

trait StatusMove extends Move {
  // Stats don't really enter the picture with Status Moves
  override val moveType = STATUSMOVE
  override val power = 0
}

/* PHYSICAL MOVES */
// PHYSICAL, HP TRANSFER
class LeechLife extends PhysicalMove {
  val index = 141
  val type1 = Bug
  val power = 20
  var maxPP = 15
  var currentPP = maxPP

  // TODO: if LL breaks a substitute, no HP is restored
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val damageDealt = pb.dc.calc(attacker, defender, this, pb)
      defender.takeDamage(damageDealt)
      damageDealt match {
        case 1 => attacker.gainHP(1)
        case _ => attacker.gainHP(damageDealt / 2)
      }
    }
  }
}

// PHYSICAL, SINGLE-STRIKE DAMAGE ONLY
trait PhysicalSingleStrike extends PhysicalMove {
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
      if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val damageDealt = pb.dc.calc(attacker, defender, this, pb)
      defender.takeDamage(damageDealt)
    }
  }
}

class Pound extends PhysicalSingleStrike {
  val index = 1
  val type1 = Normal
  val power = 40
  var maxPP = 35
  var currentPP = maxPP
}

class DrillPeck extends PhysicalSingleStrike {
  val index = 65
  val type1 = Flying
  val power = 80
  var maxPP = 20
  var currentPP = maxPP
}

class Peck extends PhysicalSingleStrike {
  val index = 64
  val type1 = Flying
  val power = 35
  var maxPP = 35
  var currentPP = maxPP
}

class WingAttack extends PhysicalSingleStrike {
  val index = 17
  val type1 = Flying
  val power = 35  // increased in later generations, weak in this one
  var maxPP = 35
  var currentPP = maxPP
}

class VineWhip extends PhysicalSingleStrike {
  val index = 22
  val type1 = Grass
  val power = 45  // power and maxPP increased in later generations
  var maxPP = 10
  var currentPP = maxPP
}

class Cut extends PhysicalSingleStrike {
  val index = 15
  val type1 = Normal
  val power = 50  // power and maxPP increased in later generations
  var maxPP = 30
  var currentPP = maxPP
  override val accuracy = 0.95
}

class Earthquake extends PhysicalSingleStrike {
  val index = 89
  val type1 = Ground
  val power = 100
  var maxPP = 10
  var currentPP = maxPP
}

class DizzyPunch extends PhysicalSingleStrike {
  val index = 146
  val type1 = Normal
  val power = 70
  var maxPP = 10
  var currentPP = maxPP
}

class EggBomb extends PhysicalSingleStrike {
  val index = 121
  val type1 = Normal
  val power = 100
  var maxPP = 10
  var currentPP = maxPP
  override val accuracy = 0.75
}

class HornAttack extends PhysicalSingleStrike {
  val index = 30
  val type1 = Normal
  val power = 65
  var maxPP = 25
  var currentPP = maxPP
}

class MegaKick extends PhysicalSingleStrike {
  val index = 25
  val type1 = Normal
  val power = 120
  var maxPP = 5
  var currentPP = maxPP
  override val accuracy = 0.75
}

class MegaPunch extends PhysicalSingleStrike {
  val index = 5
  val type1 = Normal
  val power = 80
  var maxPP = 20
  var currentPP = maxPP
  override val accuracy = 0.85
}

class PayDay extends PhysicalSingleStrike {
  val index = 6
  val type1 = Normal
  val power = 40
  var maxPP = 20
  var currentPP = maxPP
}

class Scratch extends PhysicalSingleStrike {
  val index = 10
  val type1 = Normal
  val power = 40
  var maxPP = 35
  var currentPP = maxPP
}

class Slam extends PhysicalSingleStrike {
  val index = 21
  val type1 = Normal
  val power = 80
  var maxPP = 20
  var currentPP = maxPP
  override val accuracy = 0.75
}

class Strength extends PhysicalSingleStrike {
  val index = 70
  val type1 = Normal
  val power = 80
  var maxPP = 15
  var currentPP = maxPP
}

class Tackle extends PhysicalSingleStrike {
  val index = 33
  val type1 = Normal
  val power = 50
  var maxPP = 35
  var currentPP = maxPP
}

class ViceGrip extends PhysicalSingleStrike {
  val index = 11
  val type1 = Normal
  val power = 55
  var maxPP = 30
  var currentPP = maxPP
}

class RockSlide extends PhysicalSingleStrike {
  val index = 157
  val type1 = Rock
  val power = 75
  var maxPP = 10
  var currentPP = maxPP
  override val accuracy = 0.9
}

class RockThrow extends PhysicalSingleStrike {
  val index = 88
  val type1 = Rock
  val power = 50
  var maxPP = 15
  var currentPP = maxPP
  override val accuracy = 0.65  // much higher in later generations
}


// PHYSICAL, SINGLE-STRIKE, SPECIAL
class QuickAttack extends PhysicalSingleStrike {
  val index = 98
  val type1 = Normal
  val power = 40
  var maxPP = 30
  var currentPP = maxPP
  override val priority = 1
}

class Slash extends PhysicalSingleStrike {
  val index = 163
  val type1 = Normal
  val power = 70
  var maxPP = 20
  var currentPP = maxPP
  override val critHitRate = HIGH
}

class KarateChop extends PhysicalSingleStrike {
  val index = 2
  val type1 = Normal    // Fighting in later generations
  val power = 50
  var maxPP = 25
  var currentPP = maxPP
  override val critHitRate = HIGH
}

class Crabhammer extends PhysicalSingleStrike {
  val index = 152
  val type1 = Water
  val power = 100
  var maxPP = 10
  var currentPP = maxPP
  override val critHitRate = HIGH
  override val accuracy = 0.9
}


/* PHYSICAL, WITH RECOIL */
abstract class PhysicalSingleStrikeRecoil extends PhysicalMove {
  // TODO: If the user of MOVE attacks first and makes itself faint due to recoil damage, the target will not attack or be subjected to recurrent damage during that round.
  // TODO: Self-inflicted recoil damage from MOVE from the previous turn can be countered if the target does not make a move on the following turn.
  // TODO: If MOVE breaks a substitute, the user will take no recoil damage.
  val recoilProportion : Double
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
      if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val damageDealt = pb.dc.calc(attacker, defender, this, pb)
      defender.takeDamage(damageDealt)
      val recoilDamage = (damageDealt * recoilProportion).toInt
      attacker.takeDamage(recoilDamage)
      if (VERBOSE) println(s"${attacker.name} took $recoilDamage recoil damage")
    }
  }
}

class Submission extends PhysicalSingleStrikeRecoil {
  val index = 66
  val type1 = Fighting
  val power = 80
  var maxPP = 25
  var currentPP = maxPP
  override val accuracy = 0.80
  override val recoilProportion = 0.25
}

class DoubleEdge extends PhysicalSingleStrikeRecoil {
  val index = 38
  val type1 = Normal
  val power = 100  // higher in later generations
  var maxPP = 15
  var currentPP = maxPP
  override val recoilProportion = 0.25
}

class TakeDown extends PhysicalSingleStrikeRecoil {
  val index = 36
  val type1 = Normal
  val power = 90
  var maxPP = 20
  var currentPP = maxPP
  override val accuracy = 0.85
  override val recoilProportion = 0.25
}

class Struggle extends PhysicalSingleStrikeRecoil {
  val index = 165
  val type1 = Normal
  val power = 50
  var maxPP = 1
  var currentPP = 1
  override val recoilProportion = 0.5   // different from others!

  override def finishUsingMove(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // Don't deduct a PP! Just log it
    pb.moveManager.updateLastMoveIndex(attacker, index)
  }
}


/* PHYSICAL: SINGLE STRIKE, POTENTIAL STATUS CHANGE */


/* PHYSICAL: SINGLE STRIKE, POTENTIAL STAT CHANGE */


/* PHYSICAL: MULTI STRIKE */
abstract class PhysicalMultiStrike extends PhysicalMove {
  // TODO: Ends if opponent faints or substitute breaks
  // TODO: Make sure that Bide and Counter only acknowledge the last attack in this sequence
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val r = Random.nextDouble
      val numStrikes = if (r < 0.375) 2
                       else if (r < (0.375 + 0.375)) 3
                       else if (r < (0.375 + 0.375 + 0.125)) 4
                       else 5
      val damageEachTime = pb.dc.calc(attacker, defender, this, pb)  // takes crit hits into account
      for (_ <- 1 to numStrikes) {
        defender.takeDamage(damageEachTime)
      }
      if (VERBOSE) println(s"$this hit ${defender.name} $numStrikes times!")
    }
  }
}

class PinMissile extends PhysicalMultiStrike {
  val index = 42
  val type1 = Bug
  val power = 14
  var maxPP = 20
  var currentPP = maxPP
  override val accuracy = 0.85
}

class Barrage extends PhysicalMultiStrike {
  val index = 140
  val type1 = Normal
  val power = 15
  var maxPP = 20
  var currentPP = maxPP
  override val accuracy = 0.85
}

class CometPunch extends PhysicalMultiStrike {
  val index = 4
  val type1 = Normal
  val power = 18
  var maxPP = 20
  var currentPP = maxPP
  override val accuracy = 0.85
}

class DoubleSlap extends PhysicalMultiStrike {
  val index = 3
  val type1 = Normal
  val power = 15
  var maxPP = 10
  var currentPP = maxPP
  override val accuracy = 0.85
}

class FuryAttack extends PhysicalMultiStrike {
  val index = 31
  val type1 = Normal
  val power = 15
  var maxPP = 20
  var currentPP = maxPP
  override val accuracy = 0.85
}

class FurySwipes extends PhysicalMultiStrike {
  val index = 154
  val type1 = Normal
  val power = 18
  var maxPP = 15
  var currentPP = maxPP
  override val accuracy = 0.80
}

class SpikeCannon extends PhysicalMultiStrike {
  val index = 131
  val type1 = Normal
  val power = 20
  var maxPP = 15
  var currentPP = maxPP
  // 100% accuracy
}


/* PHYSICAL, DOUBLE STRIKE */
abstract class PhysicalDoubleStrike extends PhysicalMove {
  // TODO: Ends if opponent faints or substitute breaks; see PhysicalMultiStrike
  // TODO: Make sure that Bide and Counter only acknowledge the last attack in this sequence; see PhysicalMultiStrike
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val numStrikes = 2
      val damageEachTime = pb.dc.calc(attacker, defender, this, pb)  // takes crit hits into account
      for (_ <- 1 to numStrikes) {
        defender.takeDamage(damageEachTime)
      }
      if (VERBOSE) println(s"$this hit ${defender.name} $numStrikes times!")
    }
  }
}

class DoubleKick extends PhysicalDoubleStrike {
  val index = 24
  val type1 = Fighting
  val power = 30
  var maxPP = 30
  var currentPP = maxPP
}

class Bonemerang extends PhysicalDoubleStrike {
  val index = 155
  val type1 = Ground
  val power = 50
  var maxPP = 10
  var currentPP = maxPP
}


/* PHYSICAL, FUNCTION OF ENVIRONMENT */
class SeismicToss extends SpecialMove {
  val index = 69
  val type1 = Fighting
  val power = 0
  var maxPP = 20
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      defender.takeDamage(attacker.level)
    }
  }
}

class SuperFang extends SpecialMove {
  val index = 162
  val type1 = Normal
  val power = 0
  var maxPP = 10
  var currentPP = maxPP
  override val accuracy = 0.90

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val damageDealt = (defender.currentHP / 2).max(1)
      defender.takeDamage(damageDealt)
    }
  }
}


/* PHYSICAL, ONE-HIT KO */
abstract class PhysicalOneHitKO extends PhysicalMove {
  // TODO: MOVE will break a Substitute if it hits
  // TODO: MOVE can be countered for infinite damage on the turn it breaks a Substitute.
  override val accuracy = 0.30
  override val power = 0
  var maxPP = 5
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statManager.getEffectiveSpeed(attacker) >= pb.statManager.getEffectiveSpeed(defender)) {
      defender.takeDamage(defender.currentHP)
    }
  }
}

class Fissure extends PhysicalOneHitKO {
  val index = 90
  val type1 = Ground
}

class Guillotine extends PhysicalOneHitKO {
  val index = 12
  val type1 = Normal
}

class HornDrill extends PhysicalOneHitKO {
  val index = 32
  val type1 = Normal
}


/* SPECIAL MOVES */
// SPECIAL, DEAL FIXED AMOUNT OF DAMAGE
abstract class SpecialConstantDamage extends SpecialMove {
  override val power = 0
  val constantDamageAmount : Int

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      defender.takeDamage(constantDamageAmount)
    }
  }
}

class DragonRage extends SpecialConstantDamage {
  val index = 82
  val type1 = Dragon
  var maxPP = 10
  var currentPP = maxPP
  override val constantDamageAmount = 40
}

class SonicBoom extends SpecialConstantDamage {
  val index = 49
  val type1 = Normal
  var maxPP = 20
  var currentPP = maxPP
  override val accuracy = 0.9
  override val constantDamageAmount = 20
}


// SPECIAL, FUNCTION OF LEVEL
class NightShade extends SpecialMove {
  val index = 101
  val type1 = Ghost
  val power = 0
  var maxPP = 15
  var currentPP = maxPP
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      defender.takeDamage(attacker.level)
    }
  }
}

class Psywave extends SpecialMove {
  val index = 149
  val type1 = Psychic
  val power = 0
  var maxPP = 15
  var currentPP = maxPP
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val damageDealt = ((Random.nextDouble + 0.5) * attacker.level).toInt
      defender.takeDamage(damageDealt)
    }
  }
}


// SPECIAL, SINGLESTRIKE WITH POTENTIAL WEIRD STUFF
abstract class SpecialSingleStrike extends SpecialMove {
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val damageDealt = pb.dc.calc(attacker, defender, this, pb)
      defender.takeDamage(damageDealt)
    }
  }
}

class Gust extends PhysicalSingleStrike {
  // TODO: Why is this listed as a Special move, given that it's type Normal?
  val index = 149
  val type1 = Normal   // Flying in later gens
  val power = 40
  var maxPP = 35
  var currentPP = maxPP
}

class TriAttack extends SpecialSingleStrike {
  // TODO: Why is this listed as a Special move, given that it's type Normal?
  val index = 161
  val type1 = Normal
  val power = 80
  var maxPP = 10
  var currentPP = maxPP
}

class HydroPump extends SpecialSingleStrike {
  val index = 56
  val type1 = Water
  val power = 120   // down to 110 in Gen IV
  var maxPP = 5
  var currentPP = maxPP
  override val accuracy = 0.8
}

class Surf extends SpecialSingleStrike {
  val index = 57
  val type1 = Water
  val power = 95   // down to 90 in Gen IV
  var maxPP = 15
  var currentPP = maxPP
}

class WaterGun extends SpecialSingleStrike {
  val index = 55
  val type1 = Water
  val power = 40
  var maxPP = 25
  var currentPP = maxPP
}

class RazorLeaf extends SpecialSingleStrike {
  val index = 75
  val type1 = Grass
  val power = 55
  var maxPP = 25
  var currentPP = maxPP
  override val accuracy = 0.95
  override val critHitRate = HIGH
}


// TODO: SWIFT: IGNORE MODS TO ACCURACY AND EVASION, HIT FLY/DIG POKEMON


// SPECIAL: TRANSFER HP
abstract class SpecialTransferHP extends SpecialMove {
  // TODO: If MOVE breaks a substitute, no HP will be restored to the user.
  val transferProportion = 0.5
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val damageDealt = pb.dc.calc(attacker, defender, this, pb)
      defender.takeDamage(damageDealt)
      damageDealt match {
        case 1 => attacker.gainHP(1)
        case _ => attacker.gainHP((damageDealt * transferProportion).toInt)
      }
    }
  }
}

class Absorb extends SpecialTransferHP {
  val index = 71
  val type1 = Grass
  val power = 20
  var maxPP = 20  // 25 in later gens
  var currentPP = maxPP
}

class MegaDrain extends SpecialTransferHP {
  val index = 72
  val type1 = Grass
  val power = 40
  var maxPP = 10  // 15 in later gens
  var currentPP = maxPP
}

class DreamEater extends SpecialMove {
  // TODO: If DreamEater breaks a substitute, no HP will be restored to the user.
  val index = 138
  val type1 = Psychic
  val power = 40
  var maxPP = 10  // 15 in later gens
  var currentPP = maxPP

  val transferProportion = 0.5

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    defender.statusAilment match {
      case Some(SLP) => {
        if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
          val damageDealt = pb.dc.calc(attacker, defender, this, pb)
          defender.takeDamage(damageDealt)
          damageDealt match {
            case 1 => attacker.gainHP(1)
            case _ => attacker.gainHP((damageDealt * transferProportion).toInt)
          }
        }
      }
      case _ => {}
    }
  }
}


// SPECIAL, DAMAGE + POTENTIAL STAT CHANGE
abstract class SpecialDamageStat extends SpecialMove {
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // TODO: fill in!
  }
}


// SPECIAL, DAMAGE + POTENTIAL STATUS CHANGE
abstract class SpecialDamageStatus extends SpecialMove {
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // TODO: fill in!
  }
}

class Thunder extends SpecialDamageStatus {
  val index = 87
  override val accuracy = 0.7          // in [0.0, 1.0]
  val type1 = Electric
  val power = 110
  var maxPP = 10
  var currentPP = maxPP

  val statusChance = 0.1
}

abstract class Thunderbolt extends SpecialDamageStatus
abstract class ThunderShock extends SpecialDamageStatus
abstract class Ember extends SpecialDamageStatus


/* STATUS MOVES */
abstract class StatusChangeAttackerStats extends StatusMove {
  val statToChange : BattleStat.Value
  val amount : Int
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (pb.statManager.canChangeOwnStats(attacker, pb)) {
	  statToChange match {
	    case ATTACK   => pb.statManager.changeAttackStage(attacker, amount)
	    case DEFENSE  => pb.statManager.changeDefenseStage(attacker, amount)
	    case SPEED    => pb.statManager.changeSpeedStage(attacker, amount)
	    case SPECIAL  => pb.statManager.changeSpecialStage(attacker, amount)
	    case ACCURACY => pb.statManager.changeAccuracyStage(attacker, amount)
	    case EVASION  => pb.statManager.changeEvasionStage(attacker, amount)
	  }
    }
  }
}

class Sharpen extends StatusChangeAttackerStats {
  val index = 159
  val type1 = Normal
  var maxPP = 30
  var currentPP = maxPP
  override val statToChange = ATTACK
  override val amount = 1
}

class Meditate extends StatusChangeAttackerStats {
  val index = 96
  val type1 = Psychic
  var maxPP = 40
  var currentPP = maxPP
  override val statToChange = ATTACK
  override val amount = 1
}

class SwordsDance extends StatusChangeAttackerStats {
  val index = 14
  val type1 = Normal
  var maxPP = 30
  var currentPP = maxPP
  override val statToChange = ATTACK
  override val amount = 2
}


class DefenseCurl extends StatusChangeAttackerStats {
  val index = 111
  val type1 = Normal
  var maxPP = 40
  var currentPP = maxPP
  override val statToChange = DEFENSE
  override val amount = 1
}

class Withdraw extends StatusChangeAttackerStats {
  val index = 110
  val type1 = Water
  var maxPP = 40
  var currentPP = maxPP
  override val statToChange = DEFENSE
  override val amount = 1
}

class Harden extends StatusChangeAttackerStats {
  val index = 106
  val type1 = Normal
  var maxPP = 30
  var currentPP = maxPP
  override val statToChange = DEFENSE
  override val amount = 1
}


class AcidArmor extends StatusChangeAttackerStats {
  val index = 151
  val type1 = Poison
  var maxPP = 40
  var currentPP = maxPP
  override val statToChange = DEFENSE
  override val amount = 2
}


class Barrier extends StatusChangeAttackerStats {
  val index = 112
  val type1 = Psychic
  var maxPP = 30
  var currentPP = maxPP
  override val statToChange = DEFENSE
  override val amount = 2
}


class DoubleTeam extends StatusChangeAttackerStats {
  val index = 104
  val type1 = Normal
  var maxPP = 15
  var currentPP = maxPP
  override val statToChange = EVASION
  override val amount = 1
}

class Minimize extends StatusChangeAttackerStats {
  val index = 107
  val type1 = Normal
  var maxPP = 20
  var currentPP = maxPP
  override val statToChange = EVASION
  override val amount = 1
}

class Agility extends StatusChangeAttackerStats {
  val index = 97
  val type1 = Psychic
  var maxPP = 30
  var currentPP = maxPP
  override val statToChange = SPEED
  override val amount = 2
}

class Growth extends StatusChangeAttackerStats {
  val index = 74
  val type1 = Normal
  var maxPP = 40
  var currentPP = maxPP
  override val statToChange = SPECIAL
  override val amount = 1
}

class Amnesia extends StatusChangeAttackerStats {
  val index = 133
  val type1 = Psychic
  var maxPP = 20
  var currentPP = maxPP
  override val statToChange = SPECIAL
  override val amount = 2
}


// STATUSMOVES that change the opponent's stats
// TODO: Make sure that the opponent's stats can actually be changed!
abstract class StatusChangeDefenderStats extends StatusMove {
  val statToChange : BattleStat.Value
  val amount : Int
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (pb.statManager.canChangeDefenderStats(attacker, defender, pb)) {
      statToChange match {
        case ATTACK   => pb.statManager.changeAttackStage(defender, amount)
        case DEFENSE  => pb.statManager.changeDefenseStage(defender, amount)
        case SPEED    => pb.statManager.changeSpeedStage(defender, amount)
        case SPECIAL  => pb.statManager.changeSpecialStage(defender, amount)
        case ACCURACY => pb.statManager.changeAccuracyStage(defender, amount)
        case EVASION  => pb.statManager.changeEvasionStage(defender, amount)
      }
    }
  }
}

class StringShot extends StatusChangeDefenderStats {
  val index = 81
  override val accuracy = .95          // in [0.0, 1.0]
  val type1 = Bug
  var maxPP = 40
  var currentPP = maxPP
  override val statToChange = SPEED
  override val amount = -1
}

class SandAttack extends StatusChangeDefenderStats {
  val index = 28
  val type1 = Normal  // changed in later Gens
  var maxPP = 15
  var currentPP = maxPP
  override val statToChange = ACCURACY
  override val amount = -1
}

class Flash extends StatusChangeDefenderStats {
  val index = 148
  val type1 = Normal
  var maxPP = 20
  var currentPP = maxPP
  override val accuracy = 0.7
  override val statToChange = ACCURACY
  override val amount = -1
}

class SmokeScreen extends StatusChangeDefenderStats {
  val index = 108
  val type1 = Normal
  var maxPP = 20
  var currentPP = maxPP
  override val statToChange = ACCURACY
  override val amount = -1
}

class Kinesis extends StatusChangeDefenderStats {
  val index = 134
  val type1 = Psychic
  var maxPP = 15
  var currentPP = maxPP
  override val accuracy = 0.8
  override val statToChange = ACCURACY
  override val amount = -1
}

class Growl extends StatusChangeDefenderStats {
  val index = 45
  val type1 = Normal
  var maxPP = 40
  var currentPP = maxPP
  override val statToChange = ATTACK
  override val amount = -1
}

class Leer extends StatusChangeDefenderStats {
  val index = 43
  val type1 = Normal
  var maxPP = 30
  var currentPP = maxPP
  override val statToChange = DEFENSE
  override val amount = -1
}

class TailWhip extends StatusChangeDefenderStats {
  val index = 39
  val type1 = Normal
  var maxPP = 30
  var currentPP = maxPP
  override val statToChange = DEFENSE
  override val amount = -1
}

class Screech extends StatusChangeDefenderStats {
  val index = 103
  val type1 = Normal
  var maxPP = 40
  var currentPP = maxPP
  override val accuracy = 0.85
  override val statToChange = DEFENSE
  override val amount = -2
}


// STATUSMOVES that change the opponent's statusAilment
class ThunderWave extends StatusMove {
  val index = 86
  val type1 = Electric
  var maxPP = 20
  var currentPP = maxPP
  val chancePAR = 1.0

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chancePAR) {
        defender.tryToChangeStatusAilment(PAR)
      }
    }
  }
}

class StunSpore extends StatusMove {
  val index = 78
  val type1 = Grass
  var maxPP = 30
  var currentPP = maxPP
  val chancePAR = 1.0
  override val accuracy = 0.75

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chancePAR) {
        defender.tryToChangeStatusAilment(PAR)
      }
    }
  }
}

class Glare extends StatusMove {
  val index = 137
  val type1 = Normal
  var maxPP = 30
  var currentPP = maxPP
  val chancePAR = 1.0
  override val accuracy = 0.75  // increased in later generations

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chancePAR) {
        defender.tryToChangeStatusAilment(PAR)
      }
    }
  }
}

class ConfuseRay extends StatusMove {
  val index = 109
  val type1 = Ghost
  var maxPP = 10
  var currentPP = maxPP
  val chanceCON = 1.0

  // TODO: ConfuseRay will fail if the target has a substitute
  // TODO: Confusion is volative and requires a data structure! Probably a map from Pokemon to number of turns left with confusion
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chanceCON) {
        // defender.tryToChangeStatusAilment(CON)
      }
    }
  }
}

//class Supersonic extends StatusMove {
//  val index = 48
//  val type1 = Normal
//  var maxPP = 20
//  var currentPP = maxPP
//  val chanceCON = 1.0
//  override val accuracy = 0.55
//
//  // TODO: Supersonic will fail if the target has a substitute
//  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
//    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
//      if (Random.nextDouble < chanceCON) {
//        defender.tryToChangeStatusAilment(CON)
//      }
//    }
//  }
//}

class SleepPowder extends StatusMove {
  val index = 79
  val type1 = Grass
  var maxPP = 15
  var currentPP = maxPP
  val chanceSLP = 1.0
  override val accuracy = 0.75

  // TODO: SleepPowder will fail if the target has a substitute
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chanceSLP) {
        defender.tryToChangeStatusAilment(SLP)
      }
    }
  }
}

class Hypnosis extends StatusMove {
  val index = 95
  val type1 = Psychic
  var maxPP = 15
  var currentPP = maxPP
  val chanceSLP = 1.0
  override val accuracy = 0.60

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chanceSLP) {
        defender.tryToChangeStatusAilment(SLP)
      }
    }
  }
}

class PoisonGas extends StatusMove {
  val index = 139
  val type1 = Poison
  var maxPP = 40
  var currentPP = maxPP
  val chancePSN = 1.0
  override val accuracy = 0.55

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chancePSN) {
        defender.tryToChangeStatusAilment(PSN)
      }
    }
  }
}

class Toxic extends StatusMove {
  val index = 92
  val type1 = Poison
  var maxPP = 10
  var currentPP = maxPP
  val chancePSN = 1.0
  override val accuracy = 0.85

  // TODO: Toxic is a mess and requires a battle data structure
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chancePSN) {
        defender.tryToChangeStatusAilment(BPSN)
      }
    }
  }
}

// SUPER WEIRD STATUS MOVES
class MirrorMove extends StatusMove {
  val index = 119
  val type1 = Flying
  var maxPP = 20
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // Get the last move that $defender used during his current stay in battle;
    val lastMove : Option[Move] = pb.moveManager.getLastMove(defender)

    // $attacker should use lastMove against $defender
    lastMove match {
      case Some(m) => m.use(attacker, defender, pb)
      case None    => {}
    }
  }

  /* MirrorMove would get the updateLastMoveIndex wrong, since the order would be:
   * MirrorMove.startUsingMove()
   * MirrorMove.moveSpecificStuff()   => uses Move m
   *   m.startUsingMove()
   *   m.moveSpecificStuff()
   *   m.finishUsingMove()            => sets lastMoveUsed to the correct value
   * Move.finishUsingMove()     => sets it back to incorrect value
   * So just don't update lastMoveUsed after calling MirrorMove!
   */
  override def finishUsingMove(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    currentPP -= 1
  }
}

class Haze extends StatusMove {
  val index = 114
  val type1 = Ice
  var maxPP = 30
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) {
    // http://bulbapedia.bulbagarden.net/wiki/Haze_(move)
    // reset the stat levels of both active Pokemon to 0
    pb.statManager.resetAll(attacker)
    pb.statManager.resetAll(defender)

    // TODO: fill in all the crazy stuff that Haze does
    // remove the stat reductions due to BRN/PAR

    // negate Focus Energy for both active Pokemon

    // negate Leech Seed for both active Pokemon

    // negate Light Screen for both active Pokemon

    // negate Mist for both active Pokemon

    // negate Reflect for both active Pokemon

    // negate confusion for both active Pokemon

    // negate Leech Seed for both active Pokemon

    // negate any major status ailments for THE ENEMY

    // Superweird thing that happens if an opponent is trying to use Hyper Beam, then gets frozen, and then Haze unfreezes him
  }
}

class Mist extends StatusMove {
  val index = 54
  val type1 = Ice
  var maxPP = 30
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // http://bulbapedia.bulbagarden.net/wiki/Mist_(move)
    // Mist protects the user from stat mods inflicted by the opponent until it switches out
    // TODO: flip the Mist switch for attacker in the Battle's Mist data structure
  }
}

class Conversion extends StatusMove {
  val index = 160
  val type1 = Normal
  var maxPP = 30
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // Conversion changes the types of the attacker to be those of the defender
    // Porygon is the only Pokemon in the game that can learn this move
    assert(attacker.index == 137, "someone other than Porygon tried to use Conversion!")
    attacker.type1 = defender.type1
    attacker.type2 = defender.type2
  }
}

class Metronome extends StatusMove {
  val index = 119
  val type1 = Flying
  var maxPP = 20
  var currentPP = maxPP

  private def getValidIndex() : Int = {
      val potentialIndex = Utils.intBetween(1, 165 + 1)
      if (potentialIndex != index && potentialIndex != 165) potentialIndex
      else getValidIndex()
  }

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // randomly selects a move (other than itself and Struggle) and then executes the attack with normal priority
    val randomIndex = getValidIndex()
    val moveToUse = MoveMaker.makeMove(randomIndex)
    // moveToUse.priority = 0    // TODO: make this work!
    moveToUse.use(attacker, defender, pb)
  }

  override def finishUsingMove(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // The random move used is logged as the most recent move used, so don't log a la MirrorMove
    currentPP -= 1
  }
}

class Mimic extends StatusMove {
  val index = 102
  val type1 = Normal
  var maxPP = 20
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // TODO: figure out what Mimic actually does in Gen 1, then make it happen
  }
}

class Recovery extends StatusMove {
  val index = 105
  val type1 = Normal
  var maxPP = 10
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    val currentHP = attacker.currentHP
    val maxHP = attacker.maxHP
    if (currentHP > maxHP) {
      attacker.currentHP = maxHP
    } else if (currentHP == maxHP) {
      // no recovering necessary
    } else if (Battle.recoverBugEnabled && ((maxHP - currentHP) + 1) % 256 == 0) {
      // bug - do nothing!
    } else {
      val hpToHeal = maxHP / 2
      attacker.gainHP(hpToHeal)
    }
  }
}

class Rest extends StatusMove {
  val index = 156
  val type1 = Psychic
  var maxPP = 10
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // http://bulbapedia.bulbagarden.net/wiki/Rest_(move)
    // On the turn that the Pokemon uses it: switch to SLP, regain all HP
    // Next turn: Pokemon is asleep, can Switch; choosing Fight causes it to tell you that Pokemon is asleep
    // Next turn: wake up at beginning of turn, can use an action
    // TODO: implement this in some data structure that tracks SLP
  }
}

class SoftBoiled extends StatusMove {
  val index = 135
  val type1 = Psychic
  var maxPP = 10
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // Same as recovery!
    val currentHP = attacker.currentHP
    val maxHP = attacker.maxHP
    if (currentHP > maxHP) {
      attacker.currentHP = maxHP
    } else if (currentHP == maxHP) {
      // no recovering necessary
    } else if (Battle.softboiledBugEnabled && ((maxHP - currentHP) + 1) % 256 == 0) {
      // bug - do nothing!
    } else {
      val hpToHeal = maxHP / 2
      attacker.gainHP(hpToHeal)
    }
  }
}

class Roar extends StatusMove {
  val index = 46
  val type1 = Normal
  var maxPP = 20
  var currentPP = maxPP
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // no effect in Gen 1 against another trainer
  }
}

class Whirlwind extends StatusMove {
  val index = 18
  val type1 = Normal
  var maxPP = 20
  var currentPP = maxPP
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // no effect in Gen 1 against another trainer
  }
}

class Teleport extends StatusMove {
  val index = 100
  val type1 = Psychic
  var maxPP = 20
  var currentPP = maxPP
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // no effect in Gen 1 against another trainer
  }
}

class Splash extends StatusMove {
  val index = 150
  val type1 = Normal
  var maxPP = 40
  var currentPP = maxPP
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // no effect in Gen 1 against anyone
  }
}

class Transform extends StatusMove {
  val index = 144
  val type1 = Normal
  var maxPP = 10
  var currentPP = maxPP
  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // http://bulbapedia.bulbagarden.net/wiki/Transform_(move)
    // http://www.smogon.com/rb/moves/Transform
    // Change the user's current type to that of the target
    attacker.type1 = defender.type1
    attacker.type2 = defender.type2

    // Change the user's current stats to that of the target
    // TODO: which stats, exactly, are duplicated? EV? IV? attack/defense?

    // Change the user's current stat modifications to that of the target
    pb.statManager.setAttackStage(attacker, pb.statManager.attackStages(defender))
    pb.statManager.setDefenseStage(attacker, pb.statManager.defenseStages(defender))
    pb.statManager.setSpecialStage(attacker, pb.statManager.specialStages(defender))
    pb.statManager.setSpeedStage(attacker, pb.statManager.speedStages(defender))
    pb.statManager.setAccuracyStage(attacker, pb.statManager.accuracyStages(defender))
    pb.statManager.setEvasionStage(attacker, pb.statManager.evasionStages(defender))

    // Change the user's current moves to those of the target
    val move1 = defender.move1 match {
      case None => None
      case Some(m) => {
        val newMove = MoveMaker.makeMove(m.index)
        newMove.maxPP = 5
        newMove.currentPP = 5
        newMove
      }
    }
    val move2 = defender.move2 match {
      case None => None
      case Some(m) => {
        val newMove = MoveMaker.makeMove(m.index)
        newMove.maxPP = 5
        newMove.currentPP = 5
        newMove
      }
    }
    val move3 = defender.move3 match {
      case None => None
      case Some(m) => {
        val newMove = MoveMaker.makeMove(m.index)
        newMove.maxPP = 5
        newMove.currentPP = 5
        newMove
      }
    }
    val move4 = defender.move4 match {
      case None => None
      case Some(m) => {
        val newMove = MoveMaker.makeMove(m.index)
        newMove.maxPP = 5
        newMove.currentPP = 5
        newMove
      }
    }
  }
}
