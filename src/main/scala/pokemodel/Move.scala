package pokemodel

import scala.collection.mutable
import Type._
import MoveType._
import StatusAilment._
import CritHitType._
import scala.util.Random

// TODO: Add the fact that only centain Pokemon can learn certain moves
// Can be scraped from pages such as
// http://bulbapedia.bulbagarden.net/wiki/Charmander_(Pok%C3%A9mon)/Generation_I_learnset

/* In the game, each Move is stored exactly once in memory, and the Pokemon Data Structure keeps
 * track of which Moves the Pokemon knows and how many PP are left for each Move the Pokemon has.
 *
 * I modeled Moves as objects that maintain their own statistics and state, and know how to do things
 * like use themselves against another Pokemon.
 *
 * I originally made the constructor value a Pokemon, but this became a Builder/Pokemon issue:
 * if a Move really needs a Pokemon to be created, then you can't really load a Move into a PokemonBuilder,
 * since the Builder hasn't built the Pokemon yet.
 *
 * With the constructor value an Option[Pokemon], the Move can start without a Pokemon (which kind of makes sense
 * to think about anyway), and then when a Pokemon is created from a Builder, it can pass itself as the Move owner.
 */

// TODO: Any move that can cause a stat change to opponent needs to make sure opponent doesn't have Mist cast

sealed trait Move {
  /* ABSTRACT STUFF */
  val index : Int                // in 1 .. 165
  val type1 : Type.Value
  val moveType : MoveType.Value
  val power : Int
  val maxPP : Int
  var currentPP : Int
  def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle)

  /* IMPEMENTED STUFF */
  val critHitRate = LOW   // True for 99% of moves, outliers can override
  var priority = 0        // True for 99% of moves, outliers can override; var because Metronome needs to change priority to 0
  val accuracy = 1.0      // in [0.0, 1.0], true for ~60% of moves, others can override
  def restorePP(amount : Int) = { currentPP = intWrapper(maxPP) min (currentPP + amount) }
  def restorePP() = { currentPP = maxPP }

  def startUsingMove(attacker: Pokemon, defender: Pokemon, pb: Battle) : Unit = {
    assert(currentPP > 0, s"tried to use $this with 0 PP")
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
class Struggle extends PhysicalMove {
  val index = 165
  val type1 = Normal
  val power = 50
  val maxPP = 1
  var currentPP = 1

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // Attack!

    // Take 1/2 damage dealt as recoil

    // DO NOT DEDUCT A PP
  }
}

class Pound extends PhysicalMove {
  val index = 1
  val type1 = Normal
  val power = 40
  val maxPP = 35
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val damageDealt = pb.dc.calc(attacker, defender, this, pb)
      defender.takeDamage(damageDealt)
      println(s"${attacker.name} dealt $damageDealt damage to ${defender.name} with $this")
    } else {
      println("Pound missed!")
    }
  }
}


/* SPECIAL MOVES */
class DragonRage extends SpecialMove {
  val index = 82
  val type1 = Dragon
  val power = 0
  val maxPP = 10
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
  val maxPP = 20
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
  val maxPP = 10
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
  val maxPP = 30
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeAttackStage(attacker, 1)
  }
}

class Meditate extends StatusMove {
  val index = 96
  val type1 = Psychic
  val maxPP = 40
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeAttackStage(attacker, 1)
  }
}

class SwordsDance extends StatusMove {
  val index = 14
  val type1 = Normal
  val maxPP = 30
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeAttackStage(attacker, 2)
  }
}


class DefenseCurl extends StatusMove {
  val index = 111
  val type1 = Normal
  val maxPP = 40
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeDefenseStage(attacker, 1)
  }
}

class Withdraw extends StatusMove {
  val index = 110
  val type1 = Water
  val maxPP = 40
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeDefenseStage(attacker, 1)
  }
}

class Harden extends StatusMove {
  val index = 106
  val type1 = Normal
  val maxPP = 30
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeDefenseStage(attacker, 1)
  }
}


class AcidArmor extends StatusMove {
  val index = 151
  val type1 = Poison
  val maxPP = 40
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeDefenseStage(attacker, 2)
  }
}


class Barrier extends StatusMove {
  val index = 112
  val type1 = Psychic
  val maxPP = 30
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeDefenseStage(attacker, 2)
  }
}


class DoubleTeam extends StatusMove {
  val index = 104
  val type1 = Normal
  val maxPP = 15
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeEvasionStage(attacker, 1)
  }
}

class Minimize extends StatusMove {
  val index = 107
  val type1 = Normal
  val maxPP = 20
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeEvasionStage(attacker, 1)
  }
}

class Agility extends StatusMove {
  val index = 97
  val type1 = Psychic
  val maxPP = 30
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeSpeedStage(attacker, 2)
  }
}

class Growth extends StatusMove {
  val index = 74
  val type1 = Normal
  val maxPP = 40
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeSpecialStage(attacker, 1)
  }
}

class Amnesia extends StatusMove {
  val index = 133
  val type1 = Psychic
  val maxPP = 20
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
  val maxPP = 40
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
  val maxPP = 15
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
  val maxPP = 20
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
  val maxPP = 30
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
  val maxPP = 30
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
  val maxPP = 10
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
//  val maxPP = 20
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
  val maxPP = 15
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
  val maxPP = 15
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
  val maxPP = 40
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
  val maxPP = 10
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
  val maxPP = 20
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
  val maxPP = 30
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
  val maxPP = 30
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
  val maxPP = 30
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
  val maxPP = 20
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
    moveToUse.priority = 0
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
  val maxPP = 20
  var currentPP = maxPP

  override def moveSpecificStuff(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // TODO: figure out what Mimic actually does in Gen 1, then make it happen
  }

}

class Recovery extends StatusMove {
  val index = 105
  val type1 = Normal
  val maxPP = 10
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

