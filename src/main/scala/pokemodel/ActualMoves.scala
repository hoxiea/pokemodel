package pokemodel

import Type._
import MoveType._
import BattleStat._
import TakeDamageResult._
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

  override def finishUsingMove(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // Don't deduct a PP! Just log it
    pb.moveManager.updateLastMoveIndex(attacker, index)
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

    val result = new MoveResultBuilder().moveIndex(index)
    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {

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
}

class HyperFang extends PhysicalMove with VolatileStatusChange with SingleStrike {
  override val index = 158
  override val power = 80
  override val maxPP = 15
  override val accuracy = 0.9
  override val statusAilmentToCause = new FLINCH
  override val chanceOfCausingAilment = 0.10
}

class Headbutt extends PhysicalMove with VolatileStatusChange with SingleStrike {
  override val index = 29
  override val power = 70
  override val maxPP = 15
  override val statusAilmentToCause = new FLINCH
  override val chanceOfCausingAilment = 0.30
}

class LowKick extends PhysicalMove with VolatileStatusChange with SingleStrike {
  override val index = 67
  override val type1 = Fighting
  override val power = 50
  override val maxPP = 20
  override val accuracy = 0.9
  override val statusAilmentToCause = new FLINCH
  override val chanceOfCausingAilment = 0.30
  // TODO: Low Kick cannot make a target with a substitute flinch.
}

class BoneClub extends PhysicalMove with VolatileStatusChange with SingleStrike {
  override val index = 125
  override val type1 = Ground
  override val power = 65
  override val maxPP = 20
  override val accuracy = 0.85
  override val statusAilmentToCause = new FLINCH
  override val chanceOfCausingAilment = 0.10
  // TODO: Bone Club cannot cause a target with a substitute to flinch.
}

class Stomp extends PhysicalMove with VolatileStatusChange with SingleStrike {
  override val index = 23
  override val power = 65
  override val maxPP = 20
  override val statusAilmentToCause = new FLINCH
  override val chanceOfCausingAilment = 0.30
  // TODO: Stomp cannot make a target with a substitute flinch.
}

class RollingKick extends PhysicalMove with VolatileStatusChange with SingleStrike {
  override val index = 27
  override val type1 = Fighting
  override val power = 60
  override val maxPP = 15
  override val accuracy = 0.85
  override val statusAilmentToCause = new FLINCH
  override val chanceOfCausingAilment = 0.30
  // TODO: Rolling Kick cannot make a target with a substitute flinch.
}

class ThunderPunch extends PhysicalMove with NonVolatileStatusChange with SingleStrike {
  override val index = 9
  override val type1 = Electric
  override val power = 75
  override val maxPP = 15
  override val statusAilmentToCause = new PAR
  override val chanceOfCausingAilment = 0.10
}

class IcePunch extends PhysicalMove with NonVolatileStatusChange with SingleStrike {
  override val index = 8
  override val type1 = Ice
  override val power = 75
  override val maxPP = 15
  override val statusAilmentToCause = new FRZ
  override val chanceOfCausingAilment = 0.10
}

class FirePunch extends PhysicalMove with NonVolatileStatusChange with SingleStrike {
  override val index = 7
  override val type1 = Fire
  override val power = 75
  override val maxPP = 15
  override val statusAilmentToCause = new BRN
  override val chanceOfCausingAilment = 0.10
}

class Lick extends PhysicalMove with NonVolatileStatusChange with SingleStrike {
  override val index = 122
  override val type1 = Ghost
  override val power = 20
  override val maxPP = 30
  override val statusAilmentToCause = new PAR
  override val chanceOfCausingAilment = 0.3
}

class BodySlam extends PhysicalMove with NonVolatileStatusChange with SingleStrike {
  override val index = 34
  override val power = 85
  override val maxPP = 15
  override val statusAilmentToCause = new PAR
  override val chanceOfCausingAilment = 0.3
}

class PoisonSting extends PhysicalMove with NonVolatileStatusChange with SingleStrike {
  override val index = 40
  override val type1 = Poison
  override val power = 15
  override val maxPP = 35
  override val statusAilmentToCause = new PSN
  override val chanceOfCausingAilment = 0.3
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

    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {
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
      result.KO(!defender.isAlive)
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
}

class Guillotine extends PhysicalMove with OneHitKO {
  override val index = 12
  override val maxPP = 5
}

class HornDrill extends PhysicalMove with OneHitKO {
  override val index = 32
  override val maxPP = 5
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
    if (Random.nextDouble < chanceHit(attacker, defender, pb) &&
        pb.statusManager.canBeHit(defender)) {
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
}

class Thunderbolt extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 85
  override val type1 = Electric
  override val maxPP = 15
  override val power = 95  // later lowered to 90
  // 100 accuracy

  override def statusAilmentToCause = new PAR
  override def chanceOfCausingAilment = 0.1
}

class ThunderShock extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 84
  override val type1 = Electric
  override val maxPP = 30
  override val power = 40
  // 100 accuracy

  override def statusAilmentToCause = new PAR
  override def chanceOfCausingAilment = 0.1
}

class Ember extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 52
  override val type1 = Fire
  override val maxPP = 25
  override val power = 40
  // 100 accuracy

  override def statusAilmentToCause = new BRN
  override def chanceOfCausingAilment = 0.1
}

class FireBlast extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 126
  override val type1 = Fire
  override val maxPP = 5
  override val power = 120
  override val accuracy = 0.85

  override def statusAilmentToCause = new BRN
  override def chanceOfCausingAilment = 0.3
}

class Flamethrower extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 53
  override val type1 = Fire
  override val maxPP = 15
  override val power = 95  // decreased to 90 later
  // 100 accuracy

  override def statusAilmentToCause = new BRN
  override def chanceOfCausingAilment = 0.1
}

class Sludge extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 124
  override val type1 = Poison
  override val maxPP = 20
  override val power = 65
  // 100 accuracy

  override def statusAilmentToCause = new PSN
  override def chanceOfCausingAilment = 0.3
}

class Smog extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 123
  override val type1 = Poison
  override val maxPP = 20
  override val power = 20  // increased later
  override val accuracy = 0.7

  override def statusAilmentToCause = new PSN
  override def chanceOfCausingAilment = 0.4
}

class Blizzard extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 59
  override val type1 = Ice
  override val maxPP = 5
  override val power = 110
  override val accuracy = 0.9  // lowered later

  override def statusAilmentToCause = new FRZ
  override def chanceOfCausingAilment = 0.1
}

class IceBeam extends SpecialMove with NonVolatileStatusChange with SingleStrike {
  override val index = 58
  override val type1 = Ice
  override val maxPP = 10
  override val power = 90
  // 100 accuracy

  override def statusAilmentToCause = new FRZ
  override def chanceOfCausingAilment = 0.1
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
}

class Psybeam extends SpecialMove with VolatileStatusChange with SingleStrike {
  override val index = 60
  override val type1 = Psychic
  override val maxPP = 20
  override val power = 65
  // 100 accuracy

  override def statusAilmentToCause = new CONFUSED
  override def chanceOfCausingAilment = 0.1
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


// SPECIAL, WEIRD
class NightShade extends SpecialMove with DamageEqualsUserLevel {
  override val index = 101
  override val type1 = Ghost
  override val maxPP = 15
  // no power, 100% accuracy
}

// class Psywave extends SpecialMove {
//   override val index = 149
//   override val type1 = Psychic
//   override val power = 0
//   override val maxPP = 15

//   override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
//     // TODO: Fill in Psywave implementation
//   }
// }

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
  override def requiredStatusAilments: Set[StatusAilment] = Set(SLP())
  // 100% accuracy
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
