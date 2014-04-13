package pokemodel

import Type._
import MoveType._
import BattleStat._
import TakeDamageResult._
import ViolentStruggleType._
import CritHitType._
import scala.util.Random

/*
 * This is where the actual game Moves live - Move.scala was getting pretty
 * crowded with the traits piling up and the test Moves
 */

/******** PHYSICAL MOVES ********/
/* PHYSICAL, WITH RECOIL */
// TODO: see if Submission, DoubleEdge, TakeDown also have substitute stuff
class Struggle extends PhysicalMove with Recoil with SingleStrike {
  // TODO: weird stuff with substitute, etc.
  override val index = 165
  override val power = 50
  override val maxPP = 999
  override val recoilProportion = 0.5   // different from others!

  override def finishUsingMove(
      attacker: Pokemon,
      attackerMoveSlot: Int,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder) = {
    // Don't deduct a PP! Just log it
    pb.moveManager.updateLastMoveIndex(attacker, index)
    mrb
  }
}

class Submission extends PhysicalMove with Recoil with SingleStrike {
  override val index = 66
  override val type1 = Fighting
  override val power = 80
  override val maxPP = 25
  override val accuracy = 0.80
  override val recoilProportion = 0.25
}

class DoubleEdge extends PhysicalMove with Recoil with SingleStrike {
  override val index = 38
  override val power = 100  // higher in later generations
  override val maxPP = 15
  override val recoilProportion = 0.25
}

class TakeDown extends PhysicalMove with Recoil with SingleStrike {
  override val index = 36
  override val power = 90
  override val maxPP = 20
  override val accuracy = 0.85
  override val recoilProportion = 0.25
}


/* PHYSICAL, VANILLA SINGLESTRIKE */
class Pound extends PhysicalMove with SingleStrike {
  override val index = 1
  override val power = 40
  override val maxPP = 35
}

class Tackle extends PhysicalMove with SingleStrike {
  override val index = 33
  override val power = 50
  override val maxPP = 35
}

class DrillPeck extends PhysicalMove with SingleStrike {
  override val index = 65
  override val type1 = Flying
  override val power = 80
  override val maxPP = 20
}

class Peck extends PhysicalMove with SingleStrike {
  override val index = 64
  override val type1 = Flying
  override val power = 35
  override val maxPP = 35
}

class WingAttack extends PhysicalMove with SingleStrike {
  override val index = 17
  override val type1 = Flying
  override val power = 35  // increased in later generations, weak in this one
  override val maxPP = 35
}

class Cut extends PhysicalMove with SingleStrike {
  override val index = 15
  override val power = 50  // power and maxPP increased in later generations
  override val maxPP = 30
  override val accuracy = 0.95
}

class Earthquake extends PhysicalMove with SingleStrike {
  override val index = 89
  override val type1 = Ground
  override val power = 100
  override val maxPP = 10
}

class DizzyPunch extends PhysicalMove with SingleStrike {
  override val index = 146
  override val power = 70
  override val maxPP = 10
}

class EggBomb extends PhysicalMove with SingleStrike {
  override val index = 121
  override val power = 100
  override val maxPP = 10
  override val accuracy = 0.75
}

class HornAttack extends PhysicalMove with SingleStrike {
  override val index = 30
  override val power = 65
  override val maxPP = 25
}

class MegaKick extends PhysicalMove with SingleStrike {
  override val index = 25
  override val power = 120
  override val maxPP = 5
  override val accuracy = 0.75
}

class MegaPunch extends PhysicalMove with SingleStrike {
  override val index = 5
  override val power = 80
  override val maxPP = 20
  override val accuracy = 0.85
}

class PayDay extends PhysicalMove with SingleStrike {
  override val index = 6
  override val power = 40
  override val maxPP = 20
}

class Scratch extends PhysicalMove with SingleStrike {
  override val index = 10
  override val power = 40
  override val maxPP = 35
}

class Slam extends PhysicalMove with SingleStrike {
  override val index = 21
  override val power = 80
  override val maxPP = 20
  override val accuracy = 0.75
}

class Strength extends PhysicalMove with SingleStrike {
  override val index = 70
  override val power = 80
  override val maxPP = 15
}

class ViceGrip extends PhysicalMove with SingleStrike {
  override val index = 11
  override val power = 55
  override val maxPP = 30
}

class RockSlide extends PhysicalMove with SingleStrike {
  override val index = 157
  override val type1 = Rock
  override val power = 75
  override val maxPP = 10
  override val accuracy = 0.9
}

class RockThrow extends PhysicalMove with SingleStrike {
  override val index = 88
  override val type1 = Rock
  override val power = 50
  override val maxPP = 15
  override val accuracy = 0.65  // much higher in later generations
}

class Gust extends PhysicalMove with SingleStrike {
  override val index = 16
  override val power = 40
  override val maxPP = 35
  // (type1 == Flying) => SpecialMove in later generations
  // accuracy of 1.0
}

class TriAttack extends PhysicalMove with SingleStrike {
  override val index = 161
  override val power = 80
  override val maxPP = 10
  // Normal, accuracy of 1.0
}


/* PHYSICAL, SINGLESTRIKE, WEIRD */
class KarateChop extends PhysicalMove with SingleStrike {
  override val index = 2
  // Fighting in later generations, but it was Normal in Gen1
  override val power = 50
  override val maxPP = 25
  override val critHitRate = HIGH
}

class QuickAttack extends PhysicalMove with SingleStrike {
  override val index = 98
  override val power = 40
  override val maxPP = 30
  override val priority = 1
}

class Slash extends PhysicalMove with SingleStrike {
  override val index = 163
  override val power = 70
  override val maxPP = 20
  override val critHitRate = HIGH
}

class Crabhammer extends PhysicalMove with SingleStrike {
  override val index = 152
  override val type1 = Water
  override val power = 100
  override val maxPP = 10
  override val critHitRate = HIGH
  override val accuracy = 0.9
}

class Counter extends PhysicalMove {
  override val index = 68
  override val type1 = Fighting
  override val maxPP = 10
  override val priority = -1

  override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    if (pb.moveHits(attacker, defender, this)) {
      // Get the last MoveResult from the battle
      val mr: MoveResult = pb.moveHistory.mostRecent
      if (mr.moveType == Normal || mr.moveType == Fighting) {
        val damageToDeal = mr.damageDealt * 2  // here comes the pain
        val damageResult = defender.takeDamage(damageToDeal)

        // update result
        result.damageDealt(damageToDeal)
        result.numTimesHit(1)
        // hpGained, critHit, STAB, typeMult are irrelevant
        result.moveType(type1)
        // Even if it was a Normal move that causes status change, Counter just does damage
        result.processTakeDamageResult(defender, damageResult)
        result.merge(mrb)
        super.moveSpecificStuff(attacker, defender, pb, result)
      } else {
        // attack type was wrong, so just pass along what you received, keeping moveIndex
        result.merge(mrb)
        super.moveSpecificStuff(attacker, defender, pb, result)
      }
    } else {
      // attack missed, so just pass along what you received, keeping moveIndex
      result.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, result)
    }
  }
}


/* PHYSICAL, MULTISTRIKE */
class PinMissile extends PhysicalMove with MultiStrike {
  override val index = 42
  override val type1 = Bug
  override val power = 14
  override val maxPP = 20
  override val accuracy = 0.85
}

class Barrage extends PhysicalMove with MultiStrike {
  override val index = 140
  override val power = 15
  override val maxPP = 20
  override val accuracy = 0.85
}

class CometPunch extends PhysicalMove with MultiStrike {
  override val index = 4
  override val power = 18
  override val maxPP = 20
  override val accuracy = 0.85
}

class DoubleSlap extends PhysicalMove with MultiStrike {
  override val index = 3
  override val power = 15
  override val maxPP = 10
  override val accuracy = 0.85
}

class FuryAttack extends PhysicalMove with MultiStrike {
  override val index = 31
  override val power = 15
  override val maxPP = 20
  override val accuracy = 0.85
}

class FurySwipes extends PhysicalMove with MultiStrike {
  override val index = 154
  override val power = 18
  override val maxPP = 15
  override val accuracy = 0.80
}

class SpikeCannon extends PhysicalMove with MultiStrike {
  override val index = 131
  override val power = 20
  override val maxPP = 15
  // 100% accuracy
}


/* PHYSICAL, DOUBLE STRIKE */
class DoubleKick extends PhysicalMove with DoubleStrike {
  override val index = 24
  override val type1 = Fighting
  override val power = 30
  override val maxPP = 30
}

class Bonemerang extends PhysicalMove with DoubleStrike {
  override val index = 155
  override val type1 = Ground
  override val power = 50
  override val maxPP = 10
}


/* PHYSICAL, TRANSFER HP */
class LeechLife extends PhysicalMove with GainPropDamageDealt with SingleStrike {
  override val index = 141
  override val type1 = Bug
  override val power = 20
  override val maxPP = 15
  // 100% accuracy
}


/*
 * PHYSICAL, SINGLE STRIKE + POTENTIAL STATUS CHANGE
 * Mix in SingleStrike first, since if you miss your strike, you don't
 * need to worry about causing a status change.
 */

class Bite extends PhysicalMove with VolatileStatusChange with SingleStrike {
  override val index = 44
  override val power = 60
  override val maxPP = 25
  override val statusAilmentToCause = new FLINCH
  override val chanceOfCausingAilment = 0.10
  def soloStatusChange = false
  def worksWhenSubPresent = false
}

class HyperFang extends PhysicalMove with VolatileStatusChange with SingleStrike {
  override val index = 158
  override val power = 80
  override val maxPP = 15
  override val accuracy = 0.9
  override val statusAilmentToCause = new FLINCH
  override val chanceOfCausingAilment = 0.10
  def soloStatusChange = false
  def worksWhenSubPresent = false
}

class Headbutt extends PhysicalMove with VolatileStatusChange with SingleStrike {
  override val index = 29
  override val power = 70
  override val maxPP = 15
  override val statusAilmentToCause = new FLINCH
  override val chanceOfCausingAilment = 0.30
  def soloStatusChange = false
  def worksWhenSubPresent = false
}

class LowKick extends PhysicalMove with VolatileStatusChange with SingleStrike {
  override val index = 67
  override val type1 = Fighting
  override val power = 50
  override val maxPP = 20
  override val accuracy = 0.9
  override val statusAilmentToCause = new FLINCH
  override val chanceOfCausingAilment = 0.30
  def soloStatusChange = false
  def worksWhenSubPresent = false
}

class BoneClub extends PhysicalMove with VolatileStatusChange with SingleStrike {
  override val index = 125
  override val type1 = Ground
  override val power = 65
  override val maxPP = 20
  override val accuracy = 0.85
  override val statusAilmentToCause = new FLINCH
  override val chanceOfCausingAilment = 0.10
  def soloStatusChange = false
  def worksWhenSubPresent = false
}

class Stomp extends PhysicalMove with VolatileStatusChange with SingleStrike {
  override val index = 23
  override val power = 65
  override val maxPP = 20
  override val statusAilmentToCause = new FLINCH
  override val chanceOfCausingAilment = 0.30
  def soloStatusChange = false
  def worksWhenSubPresent = false
}

class RollingKick extends PhysicalMove with VolatileStatusChange with SingleStrike {
  override val index = 27
  override val type1 = Fighting
  override val power = 60
  override val maxPP = 15
  override val accuracy = 0.85
  override val statusAilmentToCause = new FLINCH
  override val chanceOfCausingAilment = 0.30
  def soloStatusChange = false
  def worksWhenSubPresent = false
}

class ThunderPunch extends PhysicalMove with NonVolatileStatusChange with SingleStrike {
  override val index = 9
  override val type1 = Electric
  override val power = 75
  override val maxPP = 15
  override val statusAilmentToCause = new PAR
  override val chanceOfCausingAilment = 0.10
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

class IcePunch extends PhysicalMove with NonVolatileStatusChange with SingleStrike {
  override val index = 8
  override val type1 = Ice
  override val power = 75
  override val maxPP = 15
  override val statusAilmentToCause = new FRZ
  override val chanceOfCausingAilment = 0.10
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

class FirePunch extends PhysicalMove with NonVolatileStatusChange with SingleStrike {
  override val index = 7
  override val type1 = Fire
  override val power = 75
  override val maxPP = 15
  override val statusAilmentToCause = new BRN
  override val chanceOfCausingAilment = 0.10
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

class Lick extends PhysicalMove with NonVolatileStatusChange with SingleStrike {
  override val index = 122
  override val type1 = Ghost
  override val power = 20
  override val maxPP = 30
  override val statusAilmentToCause = new PAR
  override val chanceOfCausingAilment = 0.3
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

class BodySlam extends PhysicalMove with NonVolatileStatusChange with SingleStrike {
  override val index = 34
  override val power = 85
  override val maxPP = 15
  override val statusAilmentToCause = new PAR
  override val chanceOfCausingAilment = 0.3
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

class PoisonSting extends PhysicalMove with NonVolatileStatusChange with SingleStrike {
  override val index = 40
  override val type1 = Poison
  override val power = 15
  override val maxPP = 35
  override val statusAilmentToCause = new PSN
  override val chanceOfCausingAilment = 0.3
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

class Twineedle extends PhysicalMove {
  // This is superweird... it's DoubleStrike and StatusChange, but it needs to
  // attempt the StatusChange on both attempts. I programmed this one from
  // scratch rather that trying something like "with StatusChange with
  // StatusChange" or something equally unlikely to work.
  // TODO: Twineedle will stop after 1 if the first strike breaks a substitute.
  override val index = 41
  override val type1 = Bug
  override val power = 25
  override val maxPP = 20
  val statusAilmentToCause = new PSN
  val chanceOfCausingAilment = 0.2
  def statusAilmentCaused: Boolean = Random.nextDouble < chanceOfCausingAilment

  override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    if (pb.moveHits(attacker, defender, this)) {
      val numStrikes = 2

      // In Gen 1, damage was calculated once and then used for each blow
      val result = pb.dc.calc(attacker, defender, this, pb)
      val damageEachStrike = result.damageDealt
      val damageSeq = Utils.damageSeqCalc(numStrikes, damageEachStrike, defender.currentHP())
      assert(damageSeq.last > 0, "damageSeqCalc fail")

      // We now diverge from MultiStrike and start stealing from StatusChange
      // For each of the either 1 or 2 strikes in damageSeq, deal that damage
      // and try to cause PSN each time
      for (damage <- damageSeq) {
        defender.takeDamage(damage)
        if (statusAilmentCaused &&
            pb.statusManager.changeMajorStatusAilment(defender, statusAilmentToCause)) {
          result.nvsa(statusAilmentToCause)  // PSN is nvsa, can specialize here
        }
      }

      // Update result
      result.numTimesHit(damageSeq.length)
      result.damageDealt(damageSeq.last)
      // hpGained is irrelevant
      // dc.calc takes care of moveType, STAB, critHit, typeMult
      // statusChange handled above
      result.KO(!defender.isAlive)   // different!
      result.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, result)
    } else {
      // Attack missed, so just pass along what you received + move index
      val missResult = new MoveResultBuilder().moveIndex(index)
      missResult.merge(mrb)
      super.moveSpecificStuff(attacker, defender, pb, missResult)
    }
  }
}


/* PHYSICAL, ONE HIT KO */
class Fissure extends PhysicalMove with OneHitKO {
  override val index = 90
  override val type1 = Ground
  override val maxPP = 5
  override val accuracy = 0.3
}

class Guillotine extends PhysicalMove with OneHitKO {
  override val index = 12
  override val maxPP = 5
  override val accuracy = 0.3
}

class HornDrill extends PhysicalMove with OneHitKO {
  override val index = 32
  override val maxPP = 5
  override val accuracy = 0.3
}


/* PHYSICAL, SUICIDE + DAMAGE */
class Explosion extends PhysicalMove with SuicideDamage {
  override val index = 153
  override val power = 340   // Doubled, to take halved defense into account
  override val maxPP = 5
  // Normal, 100% accuracy
}

class Selfdestruct extends PhysicalMove with SuicideDamage {
  override val index = 120
  override val power = 260   // Doubled, to take halved defense into account
  override val maxPP = 5
  // Normal, 100% accuracy
}


/* PHYSICAL, POTENTIAL STAT CHANGE */
class Constrict extends PhysicalMove with EnemyStatChange with SingleStrike {
  override val index = 132
  override val maxPP = 35
  override val power = 10
  // Normal, 100% accuracy

  def statToChange = SPEED
  def amountToChangeBy = -1
  def chanceOfStatChange = 0.1
  def soloStatChange = false
}


/* PHYSICAL, FUNCTION OF ENVIRONMENT */
class SeismicToss extends PhysicalMove with DamageEqualsUserLevel {
  override val index = 69
  override val type1 = Fighting
  override val maxPP = 20
}

class SuperFang extends PhysicalMove {
  override val maxPP = 10
  override val accuracy = 0.9

  override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    if (pb.moveHits(attacker, defender, this)) {
      // The damage is not altered by weakness, resistance, or immunity.
      // Doesn't receive STAB.
      val damageToDeal = (defender.currentHP() / 2) max 1
      assert (damageToDeal <= defender.currentHP())
      val damageResult = defender.takeDamage(damageToDeal)

      // Build an MRB from scratch, since we skipped damage calculator
      result.damageCalc(damageToDeal)
      result.numTimesHit(1)
      result.damageDealt(damageToDeal)
      // no hpGained, critHit, STAB/mult, SA, stat
      result.processTakeDamageResult(defender, damageResult)
    }
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}


/* PHYSICAL, SINGLE STRIKE WITH PENALTY OF MISSING */
class HiJumpKick extends PhysicalMove with SingleStrikeLoseHPOnMiss {
  override val index = 136
  override val maxPP = 10
  override val power = 85
  override val accuracy = 0.9
  override def hpToLoseOnMiss = 1
  override def typesMissAgainst = Set(Ghost)
}

class JumpKick extends PhysicalMove with SingleStrikeLoseHPOnMiss {
  override val index = 26
  override val maxPP = 10
  override val power = 100
  override val accuracy = 0.95
  override def hpToLoseOnMiss = 1
  override def typesMissAgainst = Set(Ghost)
}


/* PHYSICAL, CUSTOM AND WEIRD */
class Bide extends PhysicalMove {
  override val index = 117
  override val maxPP = 10
  override val priority = 1
  // no accurary: works pretty much everywhere
  // no power: absorbs damage, then deals it back
  // Normal type

  override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

class Rage extends PhysicalMove {
  override val index = 99
  override val maxPP = 20
  override val power = 20
  // Normal, 100 accuracy

  // TODO: implement Rage
  override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

/*
 * Thrash actually requires 2 different moves to capture its behavior, based
 * on how I implemented things:
 * 1. RegisterThrash gets that Pokemon registered to use Thrash with the
 *    battle's WeirdMoveStatusManager. This takes care of figuring out how
 *    the thrashing will last, decrementing things as it thrashes, causing
 *    confusion once things run out, etc. It's also where the Battle will
 *    look to see if the trainer whose Pokemon used Thrash has control of
 *    his Pokemon (hint: he doesn't).
 * 2. Once Thrash is registered, we need an actual damage-causing move.
 *    The damage-causing comes from essentially a SingleStrike move with a
 *    type, accuracy, the ability to miss, etc.
 */
class RegisterThrash extends PhysicalMove with RegisterViolentStruggle {
  override val index = 37
  override val maxPP = 10
}

class Thrash extends PhysicalMove with SingleStrike with ViolentStruggleAttack {
  // SingleStrike takes care of dealing damage via moveSpecificStuff
  // ViStrAt takes care of logging and decrementing #turns Thrashing left via 
  //   startUsingMove and finishUsingMove
  override val index = 37
  override val maxPP = 999
  override val power = 120
  // Normal, 100 accuracy
}

class RegisterPetalDance extends SpecialMove with RegisterViolentStruggle {
  // TODO: Move to SpecialMove section
  override val index = 80
  override val type1 = Grass
  override val maxPP = 10
}

class PetalDance extends SpecialMove {
  // TODO: Move to SpecialMove section
  override val index = 80
  override val type1 = Grass
  override val maxPP = 999
  override val power = 120
  // 100 accuracy
}


/* WAIT/CHARGE/DIG/FLY/ETC, THEN ATTACK */
// Registrations: the maxPP matters, as you can only register if you have PP,
// even though PP is deducted upon successfully completing the Move
class RegisterDig extends PhysicalMove with RegisterWaitThenAttack {
  override val index = 91   // so that canLearnMove still works
  override val maxPP = 10
}

class RegisterFly extends PhysicalMove with RegisterWaitThenAttack {
  override val index = 19
  override val maxPP = 15
}

class RegisterSkyAttack extends PhysicalMove with RegisterWaitThenAttack {
  override val index = 143
  override val maxPP = 5
}

class RegisterSkullBash extends PhysicalMove with RegisterWaitThenAttack {
  override val index = 130
  override val maxPP = 10
}

class RegisterSolarBeam extends SpecialMove with RegisterWaitThenAttack {
  // TODO: Move into SpecialMove section
  override val index = 76
  override val maxPP = 10
}

class RegisterRazorWind extends SpecialMove with RegisterWaitThenAttack {
  override val index = 13
  override val maxPP = 10
}


// Attacks: assuming that registration succeeded using the appropriate move
// above, and assuming the sequence wasn't interrupted, this is the attack
// that gets used
class Dig extends PhysicalMove with SingleStrike with AttackAfterWaiting {
  // The damage-dealing part of Dig (turn 2)
  override val index = 91
  override val type1 = Ground
  override val maxPP = 10
  override val power = 100  // lower in later games
  // 100 accuracy
}

class SkyAttack extends PhysicalMove with SingleStrike with AttackAfterWaiting {
  // The damage-dealing part of SkyAttack (turn 2)
  override val index = 143
  override val type1 = Flying
  override val power = 140
  override val accuracy = 0.95
}

class SkullBash extends PhysicalMove with SingleStrike with AttackAfterWaiting {
  // The damage-dealing part of SkullBash (turn 2)
  override val index = 130
  override val power = 130
  // Normal, accuracy 100
}

class SolarBeam extends SpecialMove with SingleStrike with AttackAfterWaiting {
  // The damage-dealing part of SolarBeam (turn 2)
  override val index = 76
  override val type1 = Grass
  override val power = 120
  // accuracy 100
}

class RazorWind extends SpecialMove with SingleStrike with AttackAfterWaiting {
  // The damage-dealing part of RazorWind (turn 2)
  override val index = 13
  override val type1 = Grass
  override val power = 80
  override val accuracy = 0.75   // much higher later
  // Normal
}

class Fly extends PhysicalMove with SingleStrike with AttackAfterWaiting {
  // The damage-dealing part of Fly (turn 2)
  override val index = 19
  override val type1 = Flying
  override val power = 90
  override val accuracy = 0.95
}


class HyperBeam extends PhysicalMove with SingleStrike {
  /*
   * HyperBeam is essentially a Normal-type SingleStrike move, except that
   * there's sometimes a delay turn afterwards. So we extend SingleStrike to
   * get the damage-dealing part and then override finishUsingMove to register
   * the potential delay.
   *
   * In Gen1, HyperBeam doesn't require a recharge turn if:
   * - it misses
   * - breaks a substitute
   * - KOs the opponent
   * - TODO: weird glitchy stuff
   *
   * This skip-recharge stuff was all eliminated in Gen 2, so there's a
   * flag, Glitch.hyperbeamRechargeGlitch to switch this on/off
   */

  override val index = 63
  override val maxPP = 5
  override val power = 150
  override val accuracy = 0.9

  override def finishUsingMove(
      attacker: Pokemon,
      attackerMoveSlot: Int,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder) = {
    // Standard stuff
    pb.moveManager.updateLastMoveIndex(attacker, index)
    attacker.deductPP(attackerMoveSlot)

    // Register delay
    def delayNotNeeded =
      Glitch.hyperbeamRechargeGlitch &&
      (mrb.numTimesHit == 0 || mrb.subKO || mrb.KO)

    if (!delayNotNeeded) {
      val success = pb.weirdMoveStatusManager.tryToRegisterHyperBeamDelay(attacker)
      if (!success)
        throw new Exception("tried to register HyperBeam, but registration failed")
      assert(pb.weirdMoveStatusManager.hasHyperBeamDelay(attacker))
    }

    // Pay it forward
    mrb
  }

}

class Bind extends PhysicalMove with PartiallyTrapping {
  override val index = 20
  override val maxPP = 20
  override val power = 15
  override val accuracy = 0.85
  // Normal
}

class Wrap extends PhysicalMove with PartiallyTrapping {
  override val index = 35
  override val maxPP = 20
  override val power = 15
  override val accuracy = 0.9
  // Normal
}

class Clamp extends SpecialMove with PartiallyTrapping {
  // TODO: Move to Special section
  override val index = 128
  override val type1 = Water
  override val maxPP = 10
  override val power = 35
  override val accuracy = 0.75  // higher later
}

class FireSpin extends SpecialMove with PartiallyTrapping {
  // TODO: Move to Special section
  override val index = 83
  override val type1 = Fire
  override val maxPP = 15
  override val power = 15
  override val accuracy = 0.70
}

class Swift extends PhysicalMove {
  override val index = 129
  override val maxPP = 20
  override val power = 60

  override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    // Swift ignores accuracy and evasion
    // It always just does a damage calculation and deals it out
    // This is the "hit" logic from SingleStrike:
    val result = pb.dc.calc(attacker, defender, this, pb)
    val damageResult = defender.takeDamage(result.damageDealt)

    // Update result values
    // numTimesHit == 1, no hpGained, no statusAilments, no stat changes
    result.processTakeDamageResult(defender, damageResult)

    // Combine anything passed in from traits further to the right
    result.merge(mrb)

    // Pass at all along to the next trait/class
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

/******** SPECIAL MOVES ********/
// SPECIAL, SINGLE STRIKE
class HydroPump extends SpecialMove with SingleStrike {
  override val index = 56
  override val type1 = Water
  override val power = 120   // down to 110 in Gen IV
  override val maxPP = 5
  override val accuracy = 0.8
}

class Surf extends SpecialMove with SingleStrike {
  override val index = 57
  override val type1 = Water
  override val power = 95   // down to 90 in Gen IV
  override val maxPP = 15
}

class WaterGun extends SpecialMove with SingleStrike {
  override val index = 55
  override val type1 = Water
  override val power = 40
  override val maxPP = 25
}

class Waterfall extends SpecialMove with SingleStrike {
  // caused FLINCH in later generations
  override val index = 127
  override val type1 = Water
  override val maxPP = 15
  override val power = 80
  // 100 accuracy
}

class VineWhip extends SpecialMove with SingleStrike {
  override val index = 22
  override val type1 = Grass
  override val power = 35  // higher in later gens
  override val maxPP = 10  // much higher in later gens
  // 100% accuracy
}

class RazorLeaf extends SpecialMove with SingleStrike {
  override val index = 75
  override val type1 = Grass
  override val power = 55
  override val maxPP = 25
  override val accuracy = 0.95
  override val critHitRate = HIGH
}


// SPECIAL, CONSTANT DAMAGE
class DragonRage extends SpecialMove with ConstantDamage {
  override val index = 82
  override val type1 = Dragon
  override val maxPP = 10
  override def damageAmount = 40
}

class SonicBoom extends SpecialMove with ConstantDamage {
  override val index = 49
  override val maxPP = 20
  override val accuracy = 0.9
  override def damageAmount = 20
}


// SPECIAL, SINGLE STRIKE + POTENTIAL NONVOLATILE STATUS CHANGE
class Thunder extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 87
  override val type1 = Electric
  override val power = 110
  override val maxPP = 10
  override val accuracy = 0.7

  override def statusAilmentToCause = new PAR
  override def chanceOfCausingAilment = 0.1
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

class Thunderbolt extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 85
  override val type1 = Electric
  override val maxPP = 15
  override val power = 95  // later lowered to 90
  // 100 accuracy

  override def statusAilmentToCause = new PAR
  override def chanceOfCausingAilment = 0.1
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

class ThunderShock extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 84
  override val type1 = Electric
  override val maxPP = 30
  override val power = 40
  // 100 accuracy

  override def statusAilmentToCause = new PAR
  override def chanceOfCausingAilment = 0.1
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

class Ember extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 52
  override val type1 = Fire
  override val maxPP = 25
  override val power = 40
  // 100 accuracy

  override def statusAilmentToCause = new BRN
  override def chanceOfCausingAilment = 0.1
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

class FireBlast extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 126
  override val type1 = Fire
  override val maxPP = 5
  override val power = 120
  override val accuracy = 0.85

  override def statusAilmentToCause = new BRN
  override def chanceOfCausingAilment = 0.3
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

class Flamethrower extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 53
  override val type1 = Fire
  override val maxPP = 15
  override val power = 95  // decreased to 90 later
  // 100 accuracy

  override def statusAilmentToCause = new BRN
  override def chanceOfCausingAilment = 0.1
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

class Sludge extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 124
  override val type1 = Poison
  override val maxPP = 20
  override val power = 65
  // 100 accuracy

  override def statusAilmentToCause = new PSN
  override def chanceOfCausingAilment = 0.3
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

class Smog extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 123
  override val type1 = Poison
  override val maxPP = 20
  override val power = 20  // increased later
  override val accuracy = 0.7

  override def statusAilmentToCause = new PSN
  override def chanceOfCausingAilment = 0.4
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

class Blizzard extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 59
  override val type1 = Ice
  override val maxPP = 5
  override val power = 110
  override val accuracy = 0.9  // lowered later

  override def statusAilmentToCause = new FRZ
  override def chanceOfCausingAilment = 0.1
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

class IceBeam extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 58
  override val type1 = Ice
  override val maxPP = 10
  override val power = 90
  // 100 accuracy

  override def statusAilmentToCause = new FRZ
  override def chanceOfCausingAilment = 0.1
  override def soloStatusChange = false
  override val worksWhenSubPresent = true
}

// SPECIAL, SINGLE STRIKE + POTENTIAL VOLATILE STATUS CHANGE
class Confusion extends SpecialMove with VolatileStatusChange with SingleStrike {
  override val index = 93
  override val type1 = Psychic
  override val maxPP = 25
  override val power = 50
  // 100 accuracy

  override def statusAilmentToCause = new CONFUSED
  override def chanceOfCausingAilment = 0.1
  def soloStatusChange = false
  def worksWhenSubPresent = true
}

class Psybeam extends SpecialMove with VolatileStatusChange with SingleStrike {
  override val index = 60
  override val type1 = Psychic
  override val maxPP = 20
  override val power = 65
  // 100 accuracy

  override def statusAilmentToCause = new CONFUSED
  override def chanceOfCausingAilment = 0.1
  def soloStatusChange = false
  def worksWhenSubPresent = true
}


// SPECIAL, SINGLE STRIKE + POTENTIAL ENEMY STAT CHANGE
class AuroraBeam extends SpecialMove with EnemyStatChange with SingleStrike {
  override val index = 62
  override val maxPP = 20
  override val power = 65
  override val type1 = Ice
  // 100% accuracy

  def statToChange = ATTACK
  def amountToChangeBy = -1
  def chanceOfStatChange = 0.1
  def soloStatChange = false
}

class Acid extends SpecialMove with EnemyStatChange with SingleStrike {
  // TODO: Type Poison is PhysicalMove
  override val index = 51
  override val maxPP = 30
  override val power = 40
  override val type1 = Poison
  // 100% accuracy

  def statToChange = DEFENSE
  def amountToChangeBy = -1
  def chanceOfStatChange = 0.1
  def soloStatChange = false
}

class Psychic extends SpecialMove with EnemyStatChange with SingleStrike {
  override val index = 94
  override val maxPP = 10
  override val power = 90
  override val type1 = Ice
  // 100% accuracy

  def statToChange = SPECIAL
  def amountToChangeBy = -1
  def chanceOfStatChange = 0.3
  def soloStatChange = false
}

class Bubble extends SpecialMove with EnemyStatChange with SingleStrike {
  override val index = 145
  override val maxPP = 30
  override val power = 20   // 40 in later generations
  override val type1 = Water
  // 100% accuracy

  def statToChange = SPEED
  def amountToChangeBy = -1
  def chanceOfStatChange = 0.1
  def soloStatChange = false
}

class BubbleBeam extends SpecialMove with EnemyStatChange with SingleStrike {
  override val index = 61
  override val maxPP = 20
  override val power = 65
  override val type1 = Water
  // 100% accuracy

  def statToChange = SPEED
  def amountToChangeBy = -1
  def chanceOfStatChange = 0.1
  def soloStatChange = false
}


/* ----------------------------- */
/* SPECIAL, STRIKE + TRANSFER HP */
/* ----------------------------- */
class Absorb extends SpecialMove with GainPropDamageDealt with SingleStrike {
  override val index = 71
  override val type1 = Grass
  override val power = 20
  override val maxPP = 25
  // 100% accuracy
}

class MegaDrain extends SpecialMove with GainPropDamageDealt with SingleStrike {
  override val index = 72
  override val type1 = Grass
  override val power = 40
  override val maxPP = 15
  // 100% accuracy
}

class DreamEater extends SpecialMove with GainPropDamageDealt with SingleStrike {
  override val index = 138
  override val type1 = Psychic
  override val power = 100
  override val maxPP = 15
  override def requiredNVSAs = Set(SLP())
  // 100% accuracy
}


// SPECIAL, WEIRD
class NightShade extends SpecialMove with DamageEqualsUserLevel {
  override val index = 101
  override val type1 = Ghost
  override val maxPP = 15
  // no power, 100% accuracy
}

class Psywave extends SpecialMove {
  /*
   * Psywave inflicts a random amount of damage, varying between 0.5× and 1.5×
   * the user's level. The damage can be calculated using the following
   * formula, where X is a randomly generated number between 0 and 1: (X + 0.5)
   * * level
   *
   * Always rounded down, but Psywave always deals at least 1 damage.
   *
   * Though it is a Psychic move, Psywave deals typeless damage, taking neither
   * weakness nor resistance into account, and not being affected by STAB,
   *
   * This all sounds a lot like the ConstantDamage trait, except it's not
   * actually constant. And the damageAmount function of ConstantDamage doesn't
   * take any parameters. So I'll just copy the ConstantDamage code and modify
   * it slightly
   */
  override val index = 149
  override val type1 = Psychic
  override val power = 0
  override val maxPP = 15
  override val accuracy = 0.8   // increased later

  override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    // In this case, we skip DamageCalculator and build a MRB from scratch
    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)


    // Add the effects of hitting if necessary
    if (pb.moveHits(attacker, defender, this)) {
      val damageAmount = ((Random.nextDouble + .5) * attacker.level).toInt min 1
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



/******** STATUS MOVES ********/
/* STATUS: IMPROVE YOUR OWN BATTLE STATS */
class Sharpen extends StatusMove with SelfStatChange {
  override val index = 159
  override val maxPP = 30
  override val statToChange = ATTACK
  override val amountToChangeBy = 1
}

class Meditate extends StatusMove with SelfStatChange {
  override val index = 96
  override val type1 = Psychic
  override val maxPP = 40
  override val statToChange = ATTACK
  override val amountToChangeBy = 1
}

class SwordsDance extends StatusMove with SelfStatChange {
  override val index = 14
  override val maxPP = 30
  override val statToChange = ATTACK
  override val amountToChangeBy = 2
}

class DefenseCurl extends StatusMove with SelfStatChange {
  override val index = 111
  override val maxPP = 40
  override val statToChange = DEFENSE
  override val amountToChangeBy = 1
}

class Withdraw extends StatusMove with SelfStatChange {
  override val index = 110
  override val type1 = Water
  override val maxPP = 40
  override val statToChange = DEFENSE
  override val amountToChangeBy = 1
}

class Harden extends StatusMove with SelfStatChange {
  override val index = 106
  override val maxPP = 30
  override val statToChange = DEFENSE
  override val amountToChangeBy = 1
}

class AcidArmor extends StatusMove with SelfStatChange {
  override val index = 151
  override val type1 = Poison
  override val maxPP = 40
  override val statToChange = DEFENSE
  override val amountToChangeBy = 2
}

class Barrier extends StatusMove with SelfStatChange {
  override val index = 112
  override val type1 = Psychic
  override val maxPP = 30
  override val statToChange = DEFENSE
  override val amountToChangeBy = 2
}

class DoubleTeam extends StatusMove with SelfStatChange {
  override val index = 104
  override val maxPP = 15
  override val statToChange = EVASION
  override val amountToChangeBy = 1
}

class Minimize extends StatusMove with SelfStatChange {
  override val index = 107
  override val maxPP = 20
  override val statToChange = EVASION
  override val amountToChangeBy = 1
}

class Agility extends StatusMove with SelfStatChange {
  override val index = 97
  override val type1 = Psychic
  override val maxPP = 30
  override val statToChange = SPEED
  override val amountToChangeBy = 2
}

class Growth extends StatusMove with SelfStatChange {
  override val index = 74
  override val maxPP = 40
  override val statToChange = SPECIAL
  override val amountToChangeBy = 1
}

class Amnesia extends StatusMove with SelfStatChange {
  override val index = 133
  override val type1 = Psychic
  override val maxPP = 20
  override val statToChange = SPECIAL
  override val amountToChangeBy = 2
}


/* STATUS: WEAKEN YOUR OPPONENT'S BATTLE STATS */
class StringShot extends StatusMove with EnemyStatChange {
  override val index = 81
  override val type1 = Bug
  override val maxPP = 40
  override val accuracy = .95
  override val statToChange = SPEED
  override val amountToChangeBy = -1
  override val chanceOfStatChange = 1.0
  override val soloStatChange = true
}

class SandAttack extends StatusMove with EnemyStatChange {
  override val index = 28
  override val maxPP = 15
  override val statToChange = ACCURACY
  override val amountToChangeBy = -1
  override val chanceOfStatChange = 1.0
  override val soloStatChange = true
}

class Flash extends StatusMove with EnemyStatChange {
  override val index = 148
  override val maxPP = 20
  override val accuracy = 0.7
  override val statToChange = ACCURACY
  override val amountToChangeBy = -1
  override val chanceOfStatChange = 1.0
  override val soloStatChange = true
}

class Kinesis extends StatusMove with EnemyStatChange {
  override val index = 134
  override val type1 = Psychic
  override val maxPP = 15
  override val accuracy = 0.8
  override val statToChange = ACCURACY
  override val amountToChangeBy = -1
  override val chanceOfStatChange = 1.0
  override val soloStatChange = true
}

class Growl extends StatusMove with EnemyStatChange {
  override val index = 45
  override val maxPP = 40
  override val statToChange = ATTACK
  override val amountToChangeBy = -1
  override val chanceOfStatChange = 1.0
  override val soloStatChange = true
}

class Leer extends StatusMove with EnemyStatChange {
  override val index = 43
  override val maxPP = 30
  override val statToChange = DEFENSE
  override val amountToChangeBy = -1
  override val chanceOfStatChange = 1.0
  override val soloStatChange = true
}

class TailWhip extends StatusMove with EnemyStatChange {
  override val index = 39
  override val maxPP = 30
  override val statToChange = DEFENSE
  override val amountToChangeBy = -1
  override val chanceOfStatChange = 1.0
  override val soloStatChange = true
}

class Screech extends StatusMove with EnemyStatChange {
  override val index = 103
  override val maxPP = 40
  override val accuracy = 0.85
  override val statToChange = DEFENSE
  override val amountToChangeBy = -2
  override val chanceOfStatChange = 1.0
  override val soloStatChange = true
}

class Smokescreen extends StatusMove with EnemyStatChange {
  override val index = 108
  override val maxPP = 20
  // Normal, 100 accuracy
  override val statToChange = ACCURACY
  override val amountToChangeBy = -1
  override val chanceOfStatChange = 1.0
  override val soloStatChange = true
}


// STATUS: NONVOLATILE STATUS CHANGE
class ThunderWave extends StatusMove with NonVolatileStatusChange {
  override val index = 86
  override val type1 = Electric
  override val maxPP = 20
  override val statusAilmentToCause = new PAR
  override val chanceOfCausingAilment = 1.0
  override val soloStatusChange = true
  override val worksWhenSubPresent = true
}

class StunSpore extends StatusMove with NonVolatileStatusChange {
  override val index = 78
  override val type1 = Grass
  override val maxPP = 30
  override val statusAilmentToCause = new PAR
  override val chanceOfCausingAilment = 1.0
  override val accuracy = 0.75
  override val soloStatusChange = true
  override val worksWhenSubPresent = true
}

class Glare extends StatusMove with NonVolatileStatusChange {
  override val index = 137
  override val maxPP = 30
  override val statusAilmentToCause = new PAR
  override val chanceOfCausingAilment = 1.0
  override val accuracy = 0.75  // increased in later generations
  override val soloStatusChange = true
  override val worksWhenSubPresent = true
}

class SleepPowder extends StatusMove with NonVolatileStatusChange {
  override val index = 79
  override val type1 = Grass
  override val maxPP = 15
  override val statusAilmentToCause = new SLP
  override val chanceOfCausingAilment = 1.0
  override val accuracy = 0.75
  override val soloStatusChange = true
  override val worksWhenSubPresent = false
}

class Spore extends StatusMove with NonVolatileStatusChange {
  override val index = 147
  override val type1 = Grass
  override val maxPP = 15
  override val statusAilmentToCause = new SLP
  override val chanceOfCausingAilment = 1.0
  // 100% accuracy
  override val soloStatusChange = true
  override val worksWhenSubPresent = true
}

class LovelyKiss extends StatusMove with NonVolatileStatusChange {
  override val index = 142
  override val maxPP = 10
  override val statusAilmentToCause = new SLP
  override val chanceOfCausingAilment = 1.0
  override val accuracy = 0.75
  override val soloStatusChange = true
  override val worksWhenSubPresent = true
}

class Sing extends StatusMove with NonVolatileStatusChange {
  override val index = 47
  override val maxPP = 15
  override val statusAilmentToCause = new SLP
  override val chanceOfCausingAilment = 1.0
  override val accuracy = 0.55
  override val soloStatusChange = true
  override val worksWhenSubPresent = true
}

class Hypnosis extends StatusMove with NonVolatileStatusChange {
  override val index = 95
  override val type1 = Psychic
  override val maxPP = 15
  override val statusAilmentToCause = new SLP
  override val chanceOfCausingAilment = 1.0
  override val accuracy = 0.60
  override val soloStatusChange = true
  override val worksWhenSubPresent = true
}

class PoisonGas extends StatusMove with NonVolatileStatusChange {
  override val index = 139
  override val type1 = Poison
  override val maxPP = 40
  override val statusAilmentToCause = new PSN
  override val chanceOfCausingAilment = 1.0
  override val accuracy = 0.55
  override val soloStatusChange = true
  override val worksWhenSubPresent = true
}

class PoisonPowder extends StatusMove with NonVolatileStatusChange {
  override val index = 77
  override val type1 = Poison
  override val maxPP = 35
  override val statusAilmentToCause = new PSN
  override val chanceOfCausingAilment = 1.0
  override val accuracy = 0.75
  override val soloStatusChange = true
  override val worksWhenSubPresent = true
}


// STATUS: VOLATILE STATUS CHANGE
class ConfuseRay extends StatusMove with VolatileStatusChange {
  override val index = 109
  override val type1 = Ghost
  override val maxPP = 10
  override val statusAilmentToCause = new CONFUSED
  override val chanceOfCausingAilment = 1.0
  override val soloStatusChange = true
  override val worksWhenSubPresent = false
}

class Supersonic extends StatusMove with VolatileStatusChange {
  override val index = 48
  override val maxPP = 20
  override val accuracy = 0.55
  override val statusAilmentToCause = new CONFUSED
  override val chanceOfCausingAilment = 1.0
  override val soloStatusChange = true
  override val worksWhenSubPresent = false
}


/*
 * STATUS: SHIELDS THAT BOOST DEFENSE
 * These two really just need to register themselves with the appropriate Battle
 * data structure... their logic is handled by the StatManager.
 */
class Reflect extends StatusMove {
  // TODO: The effect of Reflect is ignored by self-inflicted Confusion damage
  override val index = 115
  override val type1 = Psychic
  override val maxPP = 20
  // accuracy irrelevant, power irrelevant

  override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {
    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    val success = pb.weirdMoveStatusManager.tryToRegisterReflect(attacker)
    if (success) {
      result.numTimesHit(1)
    }
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

class LightScreen extends StatusMove {
  override val index = 113
  override val type1 = Psychic
  override val maxPP = 30
  // accuracy irrelevant, power irrelevant

  override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {
    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    val success = pb.weirdMoveStatusManager.tryToRegisterLightscreen(attacker)
    if (success) {
      result.numTimesHit(1)
    }
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

class Conversion extends StatusMove {
  override val index = 160
  override val maxPP = 30
  // Normal, accuracy irrelevant, power irrelevant

  override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {
    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    attacker.changeType1(defender.type1, this)
    attacker.changeType2(defender.type2, this)
    result.numTimesHit(1)
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

class Disable extends StatusMove {
  override val index = 50
  override val maxPP = 20
  override val accuracy = 0.55

  // Most of the Disable logic is handled in WeirdMoveStatusManager
  override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {
    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    val success = pb.weirdMoveStatusManager.tryToDisableAMove(defender, pb)
    if (success) {
      result.numTimesHit(1)
    }
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

class Mist extends StatusMove {
  override val index = 54
  override val type1 = Ice
  override val maxPP = 30

  // A user of mist really just needs to register itself as such
  // with the Battle.
  override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {
    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    val success = pb.weirdMoveStatusManager.tryToRegisterMist(attacker)
    if (success)
      result.numTimesHit(1)
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

class LeechSeed extends StatusMove with VolatileStatusChange {
  override val index = 73
  override val type1 = Grass
  override val maxPP = 10
  override val accuracy = 0.9

  /*
   * LeechSeed is the only move in the game that causes the VSA SEEDED.
   *
   * Most of the logic is captured in BattleStatusManager (BSM).
   * BSM.tryToSeed will fail on Grass Pokemon.
   *
   * Leech Seed can be removed by Haze or by switching.
   */

  override def statusAilmentToCause = new SEEDED()
  override def chanceOfCausingAilment = 1.0
  override def soloStatusChange = true
  override def worksWhenSubPresent = true  // not true in Stadium, but true here
}

class Toxic extends StatusMove with NonVolatileStatusChange {
  override val index = 92
  override val type1 = Poison
  override val maxPP = 10
  override val accuracy = 0.85

  def chanceOfCausingAilment: Double = 1.0
  def soloStatusChange: Boolean = true
  def statusAilmentToCause: pokemodel.NonVolatileStatusAilment = new BPSN()
  def worksWhenSubPresent: Boolean = true
}

class Mimic extends StatusMove {
  override val index = 102
  override val maxPP = 10
  // TODO: implement Mimic
}

class Recover extends StatusMove with RestoreHP {
  override val index = 105
  override val maxPP = 10
}

class Softboiled extends StatusMove with RestoreHP {
  override val index = 135
  override val maxPP = 10
}

class Haze extends StatusMove {
  override val index = 114
  override val type1 = Ice
  override val maxPP = 30

  override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    // http://bulbapedia.bulbagarden.net/wiki/Haze_(move)
    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)

    // reset the stat levels of both active Pokemon to 0
    pb.statManager.resetAll(attacker)
    pb.statManager.resetAll(defender)

    /*
     * TODO: remove stat reductions due to BRN/PAR? see comment below
     * It seems like curing the enemy's statusAilment below should do that for
     * the enemy. As for doing that for you, how long does it last for?  Does
     * that last until you switch out? What if you get cured and then get
     * another one?
     */

    // negate Focus Energy for both active Pokemon
    pb.weirdMoveStatusManager.tryToRemoveFocusEnergy(attacker)
    pb.weirdMoveStatusManager.tryToRemoveFocusEnergy(defender)

    // negate Leech Seed for both active Pokemon
    pb.statusManager.tryToRemoveSeeded(attacker)
    pb.statusManager.tryToRemoveSeeded(defender)

    // negate Light Screen for both active Pokemon
    pb.weirdMoveStatusManager.tryToRemoveLightscreen(attacker)
    pb.weirdMoveStatusManager.tryToRemoveLightscreen(defender)

    // negate Mist for both active Pokemon
    pb.weirdMoveStatusManager.tryToRemoveMist(attacker)
    pb.weirdMoveStatusManager.tryToRemoveMist(defender)

    // negate Reflect for both active Pokemon
    pb.weirdMoveStatusManager.tryToRemoveReflect(attacker)
    pb.weirdMoveStatusManager.tryToRemoveReflect(defender)

    // negate confusion for both active Pokemon
    pb.statusManager.tryToRemoveConfusion(attacker)
    pb.statusManager.tryToRemoveConfusion(defender)

    // negate any major status ailments for THE ENEMY
    if (Glitch.hazeNoStatusAilmentCureGlitch)
      attacker.removeStatusAilment()
    defender.removeStatusAilment()

    // TODO: the HyperBeam bug

    result.numTimesHit(1)
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

class FocusEnergy extends StatusMove {
  override val index = 116
  override val maxPP = 30

  override def moveSpecificStuff(
    attacker: Pokemon,
    defender: Pokemon,
    pb: Battle,
    mrb: MoveResultBuilder = new MoveResultBuilder()) = {
    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    val success = pb.weirdMoveStatusManager.tryToRegisterFocusEnergy(attacker)
    if (success)
      result.numTimesHit(1)
    result.merge(mrb)
    super.moveSpecificStuff(attacker, defender, pb, result)
  }
}

class Metronome extends StatusMove {
  override val index = 118
  override val maxPP = 20

    /* Bulbapedia makes a big deal out of the fact that Metronome moves are
     * used with priority=0, such that you can see, for example, a QuickAttack
     * used as the second Move of the turn. But as long as the move is used
     * when Metronome is called, it'll have priority=0, since Metronome has
     * priority=0. So nothing special needed.
     */

  private def getValidMoveIndex: Int = {
    // Metronome never picks itself or Struggle
      val potentialIndex = Utils.intBetween(1, 165 + 1)
      if (potentialIndex != index && potentialIndex != 165) potentialIndex
      else getValidMoveIndex
  }

  override def moveSpecificStuff(
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val moveIndex  = getValidMoveIndex
    val moveToUse  = MoveDepot(moveIndex)
    val moveResult = moveToUse.use(attacker, 5, defender, pb)
    val result = new MoveResultBuilder()
    result.merge(moveResult)
    result
  }

  override def finishUsingMove(
      attacker: Pokemon,
      attackerMoveSlot: Int,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder) = {
    // TODO: Don't log metronome, whatever logging system you end up using
    // Let moveToUse log itself, though, since that should count as last move used
    attacker.deductPP(attackerMoveSlot)  // this is Metronome PP, should be decremented
    mrb
  }
}

class Rest extends StatusMove {
  override val index = 156
  override val type1 = Psychic
  override val maxPP = 10

  // TODO: implement Rest, probably in the sleeping data structure
  // http://bulbapedia.bulbagarden.net/wiki/Rest_(move)
  // On the turn that the Pokemon uses it: switch to SLP, regain all HP
  // Next turn: Pokemon is asleep, can Switch; choosing Fight causes it to tell you that Pokemon is asleep
  // Next turn: wake up at beginning of turn, can use an action
}

class MirrorMove extends StatusMove {
  override val index = 119
  override val type1 = Flying
  override val maxPP = 20

  // TODO: implement MirrorMove
  /*
   * MirrorMove would get the updateLastMoveIndex wrong, since the order would be:
   * MirrorMove.startUsingMove()
   * MirrorMove.moveSpecificStuff()   => uses Move m
   *   m.startUsingMove()
   *   m.moveSpecificStuff()
   *   m.finishUsingMove()            => sets lastMoveUsed to the correct value
   * Move.finishUsingMove()     => sets it back to incorrect value
   * So just don't update lastMoveUsed after calling MirrorMove!
   */
  // override def finishUsingMove(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
  //   currentPP -= 1
  // }
}

class Transform extends StatusMove {
  override val index = 144
  override val maxPP = 10
  // TODO: implement Transform
  // TODO: Transform wears off if you switch
  // override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
  //   // http://bulbapedia.bulbagarden.net/wiki/Transform_(move)
  //   // http://www.smogon.com/rb/moves/Transform

  //   // Change the user's current type to that of the target
  //   attacker.type1 = defender.type1
  //   attacker.type2 = defender.type2

  //   // Change the user's current stats to that of the target
  //   // TODO: which stats, exactly, are duplicated? EV? IV? attack/defense?

  //   // Change the user's current stat modifications to that of the target
  //   pb.statManager.setAttackStage(attacker, pb.statManager.attackStages(defender))
  //   pb.statManager.setDefenseStage(attacker, pb.statManager.defenseStages(defender))
  //   pb.statManager.setSpecialStage(attacker, pb.statManager.specialStages(defender))
  //   pb.statManager.setSpeedStage(attacker, pb.statManager.speedStages(defender))
  //   pb.statManager.setAccuracyStage(attacker, pb.statManager.accuracyStages(defender))
  //   pb.statManager.setEvasionStage(attacker, pb.statManager.evasionStages(defender))

  //   // Change the user's current moves to those of the target
  //   val move1 = defender.move1 match {
  //     case None => None
  //     case Some(m) => {
  //       val newMove = MoveMaker.makeMove(m.index)
  //       newMove.maxPP = 5
  //       newMove.currentPP = 5
  //       newMove
  //     }
  //   }
  //   val move2 = defender.move2 match {
  //     case None => None
  //     case Some(m) => {
  //       val newMove = MoveMaker.makeMove(m.index)
  //       newMove.maxPP = 5
  //       newMove.currentPP = 5
  //       newMove
  //     }
  //   }
  //   val move3 = defender.move3 match {
  //     case None => None
  //     case Some(m) => {
  //       val newMove = MoveMaker.makeMove(m.index)
  //       newMove.maxPP = 5
  //       newMove.currentPP = 5
  //       newMove
  //     }
  //   }
  //   val move4 = defender.move4 match {
  //     case None => None
  //     case Some(m) => {
  //       val newMove = MoveMaker.makeMove(m.index)
  //       newMove.maxPP = 5
  //       newMove.currentPP = 5
  //       newMove
  //     }
  //   }
  // }
}

class Substitute extends StatusMove {
  override val index = 164
  override val maxPP = 10

  /*
   * Substitute has a many conditions and many interactions with other moves
   * Much of the logic: in the Pokemon class, under the Substitute section
   * No stat-modifying attacks work: handled by BattleStatManager.canChangeDefenderStats
   * No major statusAilments inflicted: handled by BattleStatusManager.canCauseMajorStatusAilment
   */

  override def moveSpecificStuff(
    // numTimesHit == 1  =>  Substitute created successfully
    // else numTimesHit == 0
      attacker: Pokemon,
      defender: Pokemon,
      pb: Battle,
      mrb: MoveResultBuilder = new MoveResultBuilder()) = {

    val result = new MoveResultBuilder().moveIndex(index).moveType(type1)
    if (attacker.canMakeSub) {
      attacker.makeSub()
      result.numTimesHit(1)
    }
    result.merge(mrb)
    result
  }
}

// STATUS: USELESS STUFF
class Roar extends StatusMove {
  override val index = 46
  override val maxPP = 20
}

class Whirlwind extends StatusMove {
  override val index = 18
  override val maxPP = 20
}

class Teleport extends StatusMove {
  override val index = 100
  override val type1 = Psychic
  override val maxPP = 20
}

class Splash extends StatusMove {
  override val index = 150
  override val maxPP = 40
}
