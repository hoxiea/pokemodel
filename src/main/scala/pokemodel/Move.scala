package pokemodel

import scala.collection.mutable
import Type._
import MoveType._
import StatusAilment._
import CritHitType._
import scala.util.Random
import Battle.{verbose=>VERBOSE}

// TODO: Add the fact that only certain Pokemon can learn certain moves
// Can be scraped from pages such as
// http://bulbapedia.bulbagarden.net/wiki/Charmander_(Pok%C3%A9mon)/Generation_I_learnset

/* In the game, each Move is stored exactly once in memory, and the Pokemon Data Structure keeps
 * track of which Moves the Pokemon knows and how many PP are left for each Move the Pokemon has.
 *
 * I modeled Moves as objects that maintain their own statistics and state, and know how to do things
 * like use themselves against another Pokemon.
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
  val priority = 0        // True for 99% of moves, outliers can override; var because Metronome needs to change priority to 0
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
  def getAttackStat(attacker: Pokemon, b : Battle)  = b.statManager.getEffectiveAttack(attacker)
  def getDefenseStat(defender: Pokemon, b : Battle) = b.statManager.getEffectiveDefense(defender)
  override val moveType = PHYSICAL
}

trait SpecialMove extends Move {
  def getAttackStat(attacker: Pokemon, b : Battle)  = b.statManager.getEffectiveSpecial(attacker)
  def getDefenseStat(defender: Pokemon, b : Battle)  = b.statManager.getEffectiveSpecial(defender)
  override val moveType = SPECIAL
}

trait StatusMove extends Move {
  override val moveType = STATUS
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

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val damageDealt = pb.dc.calc(attacker, defender, this, pb)
      defender.takeDamage(damageDealt)
    } else {
    }
  }
}

// PHYSICAL, SINGLE-STRIKE DAMAGE ONLY
class Pound extends PhysicalMove {
  val index = 1
  val type1 = Normal
  val power = 40
  var maxPP = 35
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val damageDealt = pb.dc.calc(attacker, defender, this, pb)
      defender.takeDamage(damageDealt)
    } else {
    }
  }
}

// PHYSICAL, SINGLE-STRIKE, SPECIAL
class QuickAttack extends PhysicalMove {
  val index = 98
  val type1 = Normal
  val power = 40
  var maxPP = 30
  var currentPP = maxPP
  override val priority = 1

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val damageDealt = pb.dc.calc(attacker, defender, this, pb)
      defender.takeDamage(damageDealt)
    }
  }
}

class Slash extends PhysicalMove {
  val index = 163
  val type1 = Normal
  val power = 70
  var maxPP = 20
  var currentPP = maxPP
  override val critHitRate = HIGH

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val damageDealt = pb.dc.calc(attacker, defender, this, pb)
      defender.takeDamage(damageDealt)
    }
  }
}

abstract class KarateChop extends PhysicalMove
abstract class Crabhammer extends PhysicalMove

// PHYSICAL, WITH RECOIL
class Struggle extends PhysicalMove {
  val index = 165
  val type1 = Normal
  val power = 50
  var maxPP = 1
  var currentPP = 1

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val damageDealt = pb.dc.calc(attacker, defender, this, pb)
      defender.takeDamage(damageDealt)
      val recoilDamage = damageDealt / 2
      attacker.takeDamage(recoilDamage)     // Take 50% damage dealt as recoil
      if (VERBOSE) println(s"${attacker.name} took $recoilDamage recoil damage")
    }
    // DO NOT DEDUCT A PP
  }
}


/* SPECIAL MOVES */
class DragonRage extends SpecialMove {
  val index = 82
  val type1 = Dragon
  val power = 0
  var maxPP = 10
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      defender.takeDamage(40)
    }
  }
}

class SonicBoom extends SpecialMove {
  val index = 49
  override val accuracy = 0.9          // in [0.0, 1.0]
  val type1 = Normal
  val power = 0
  var maxPP = 20
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      defender.takeDamage(20)
    }
  }
}

class Thunder extends SpecialMove {
  val index = 87
  override val accuracy = 0.7          // in [0.0, 1.0]
  val type1 = Electric
  val power = 110
  var maxPP = 10
  var currentPP = maxPP

  val parChance = 0.1

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // TODO: model after Pound!
  }
}


/* STATUS MOVES */
class Sharpen extends StatusMove {
  val index = 159
  val type1 = Normal
  var maxPP = 30
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeAttackStage(attacker, 1)
  }
}

class Meditate extends StatusMove {
  val index = 96
  val type1 = Psychic
  var maxPP = 40
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeAttackStage(attacker, 1)
  }
}

class SwordsDance extends StatusMove {
  val index = 14
  val type1 = Normal
  var maxPP = 30
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeAttackStage(attacker, 2)
  }
}


class DefenseCurl extends StatusMove {
  val index = 111
  val type1 = Normal
  var maxPP = 40
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeDefenseStage(attacker, 1)
  }
}

class Withdraw extends StatusMove {
  val index = 110
  val type1 = Water
  var maxPP = 40
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeDefenseStage(attacker, 1)
  }
}

class Harden extends StatusMove {
  val index = 106
  val type1 = Normal
  var maxPP = 30
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeDefenseStage(attacker, 1)
  }
}


class AcidArmor extends StatusMove {
  val index = 151
  val type1 = Poison
  var maxPP = 40
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeDefenseStage(attacker, 2)
  }
}


class Barrier extends StatusMove {
  val index = 112
  val type1 = Psychic
  var maxPP = 30
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeDefenseStage(attacker, 2)
  }
}


class DoubleTeam extends StatusMove {
  val index = 104
  val type1 = Normal
  var maxPP = 15
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeEvasionStage(attacker, 1)
  }
}

class Minimize extends StatusMove {
  val index = 107
  val type1 = Normal
  var maxPP = 20
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeEvasionStage(attacker, 1)
  }
}

class Agility extends StatusMove {
  val index = 97
  val type1 = Psychic
  var maxPP = 30
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeSpeedStage(attacker, 2)
  }
}

class Growth extends StatusMove {
  val index = 74
  val type1 = Normal
  var maxPP = 40
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeSpecialStage(attacker, 1)
  }
}

class Amnesia extends StatusMove {
  val index = 133
  val type1 = Psychic
  var maxPP = 20
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeSpecialStage(attacker, 2)
  }
}

// STATUSMOVES that change the opponent's stats
class StringShot extends StatusMove {
  val index = 81
  override val accuracy = .95          // in [0.0, 1.0]
  val type1 = Bug
  var maxPP = 40
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      pb.statManager.changeSpeedStage(defender, -1)
    }
  }
}

class SandAttack extends StatusMove {
  val index = 28
  val type1 = Normal  // changed in later Gens
  var maxPP = 15
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      pb.statManager.changeAccuracyStage(defender, -1)
    }
  }
}

abstract class Flash extends StatusMove
abstract class SmokeScreen extends StatusMove
abstract class Kinesis extends StatusMove
abstract class Growl extends StatusMove
abstract class Leer extends StatusMove
abstract class TailWhip extends StatusMove
abstract class Screech extends StatusMove

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
    // moveToUse.priority = 0
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
