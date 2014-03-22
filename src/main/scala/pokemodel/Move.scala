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

sealed trait Move {
  /* ABSTRACT STUFF */
  val index : Int                // in 1 .. 165
  val type1 : Type.Value
  val moveType : MoveType.Value
  val power : Int
  val maxPP : Int
  var currentPP : Int
  def use(attacker: Pokemon, defender: Pokemon, pb: Battle)

  /* IMPEMENTED STUFF */
  val critHitRate = LOW   // True for 99% of moves, outliers can override
  val priority = 0        // True for 99% of moves, outliers can override
  val accuracy = 1.0      // in [0.0, 1.0], true for ~60% of moves, others can override
  def restorePP(amount : Int) = { currentPP = intWrapper(maxPP) min (currentPP + amount) }
  def restorePP() = { currentPP = maxPP }

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

  override def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
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

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    assert(currentPP > 0, s"tried to use $this with 0 pp")
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      val damageDealt = pb.dc.calc(attacker, defender, this, pb)
      defender.takeDamage(damageDealt)
      println(s"${attacker.name} dealt $damageDealt damage to ${defender.name} with $this")
    } else {
      println("Pound missed!")
    }
    currentPP -= 1
  }
}


/* SPECIAL MOVES */
class DragonRage extends SpecialMove {
  val index = 82
  val type1 = Dragon
  val power = 0
  val maxPP = 10
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      defender.takeDamage(40)
    }
    currentPP -= 1
  }
}

class SonicBoom extends SpecialMove {
  val index = 49
  override val accuracy = 0.9          // in [0.0, 1.0]
  val type1 = Normal
  val power = 0
  val maxPP = 20
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      defender.takeDamage(20)
    }
    currentPP -= 1
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

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // TODO: model after Pound!
    currentPP -= 1
  }
}


/* STATUS MOVES */
class Sharpen extends StatusMove {
  val index = 159
  val type1 = Normal
  val maxPP = 30
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeAttackStage(attacker, 1)
    currentPP -= 1
  }
}

class Meditate extends StatusMove {
  val index = 96
  val type1 = Psychic
  val maxPP = 40
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeAttackStage(attacker, 1)
    currentPP -= 1
  }
}

class SwordsDance extends StatusMove {
  val index = 14
  val type1 = Normal
  val maxPP = 30
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeAttackStage(attacker, 2)
    currentPP -= 1
  }
}


class DefenseCurl extends StatusMove {
  val index = 111
  val type1 = Normal
  val maxPP = 40
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeDefenseStage(attacker, 1)
    currentPP -= 1
  }
}

class Withdraw extends StatusMove {
  val index = 110
  val type1 = Water
  val maxPP = 40
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeDefenseStage(attacker, 1)
    currentPP -= 1
  }
}

class Harden extends StatusMove {
  val index = 106
  val type1 = Normal
  val maxPP = 30
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeDefenseStage(attacker, 1)
    currentPP -= 1
  }
}


class AcidArmor extends StatusMove {
  val index = 151
  val type1 = Poison
  val maxPP = 40
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeDefenseStage(attacker, 2)
    currentPP -= 1
  }
}


class Barrier extends StatusMove {
  val index = 112
  val type1 = Psychic
  val maxPP = 30
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeDefenseStage(attacker, 2)
    currentPP -= 1
  }
}


class DoubleTeam extends StatusMove {
  val index = 104
  val type1 = Normal
  val maxPP = 15
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeEvasionStage(attacker, 1)
    currentPP -= 1
  }
}

class Minimize extends StatusMove {
  val index = 107
  val type1 = Normal
  val maxPP = 20
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeEvasionStage(attacker, 1)
    currentPP -= 1
  }
}

class Agility extends StatusMove {
  val index = 97
  val type1 = Psychic
  val maxPP = 30
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeSpeedStage(attacker, 2)
    currentPP -= 1
  }
}

class Growth extends StatusMove {
  val index = 74
  val type1 = Normal
  val maxPP = 40
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeSpecialStage(attacker, 1)
    currentPP -= 1
  }
}

class Amnesia extends StatusMove {
  val index = 133
  val type1 = Psychic
  val maxPP = 20
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    pb.statManager.changeSpecialStage(attacker, 2)
    currentPP -= 1
  }
}

// STATUSMOVES that change the opponent's stats
class StringShot extends StatusMove {
  val index = 81
  override val accuracy = .95          // in [0.0, 1.0]
  val type1 = Bug
  val maxPP = 40
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      pb.statManager.changeSpeedStage(defender, -1)
    }
    currentPP -= 1
  }
}

class SandAttack extends StatusMove {
  val index = 28
  val type1 = Normal  // changed in later Gens
  val maxPP = 15
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      pb.statManager.changeAccuracyStage(defender, -1)
    }
    currentPP -= 1
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

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chancePAR) {
        defender.tryToChangeStatusAilment(PAR)
      }
    }
    currentPP -= 1
  }
}

class StunSpore extends StatusMove {
  val index = 78
  val type1 = Grass
  val maxPP = 30
  var currentPP = maxPP
  val chancePAR = 1.0
  override val accuracy = 0.75

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chancePAR) {
        defender.tryToChangeStatusAilment(PAR)
      }
    }
    currentPP -= 1
  }
}

class Glare extends StatusMove {
  val index = 137
  val type1 = Normal
  val maxPP = 30
  var currentPP = maxPP
  val chancePAR = 1.0
  override val accuracy = 0.75  // increased in later generations

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chancePAR) {
        defender.tryToChangeStatusAilment(PAR)
      }
    }
    currentPP -= 1
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
  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chanceCON) {
        // defender.tryToChangeStatusAilment(CON)
      }
    }
    currentPP -= 1
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
//  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
//    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
//      if (Random.nextDouble < chanceCON) {
//        defender.tryToChangeStatusAilment(CON)
//      }
//    }
//    currentPP -= 1
//  }
//}

class SleepPowder extends StatusMove {
  val index = 79
  val type1 = Grass
  val maxPP = 15
  var currentPP = maxPP
  val chanceSLP = 1.0
  override val accuracy = 0.75

  // TODO: Supersonic will fail if the target has a substitute
  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chanceSLP) {
        defender.tryToChangeStatusAilment(SLP)
      }
    }
    currentPP -= 1
  }
}

class Hypnosis extends StatusMove {
  val index = 95
  val type1 = Psychic
  val maxPP = 15
  var currentPP = maxPP
  val chanceSLP = 1.0
  override val accuracy = 0.60

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chanceSLP) {
        defender.tryToChangeStatusAilment(SLP)
      }
    }
    currentPP -= 1
  }
}

class PoisonGas extends StatusMove {
  val index = 139
  val type1 = Poison
  val maxPP = 40
  var currentPP = maxPP
  val chancePSN = 1.0
  override val accuracy = 0.55

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chancePSN) {
        defender.tryToChangeStatusAilment(PSN)
      }
    }
    currentPP -= 1
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
  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    if (Random.nextDouble < chanceHit(attacker, defender, pb)) {
      if (Random.nextDouble < chancePSN) {
        defender.tryToChangeStatusAilment(BPSN)
      }
    }
    currentPP -= 1
  }
}

// SUPER WEIRD STATUS MOVES
class MirrorMove extends StatusMove {
  val index = 119
  val type1 = Flying
  val maxPP = 20
  var currentPP = maxPP

  def use(attacker: Pokemon, defender: Pokemon, pb: Battle) = {
    // Get the last move that $defender used during his current stay in battle;
    // TODO: it resets if the Pokemon switches out! So switch needs access to the battle
    val lastMove : Option[Move] = pb.moveManager.getLastMove(defender)

    // $attacker should use lastMove against $defender
    lastMove match {
      case Some(m) => m.use(attacker, defender, pb)
      case None    => {}
    }
    currentPP -= 1
  }
}


