package pokemodel

import Type._
import MoveType._
import BattleStat._
import CritHitType._

/*
 * This is where the actual game Moves live - Move.scala was getting pretty crowded
 * with the traits piling up and the test Moves
 */

/******** PHYSICAL MOVES ********/
/* PHYSICAL, WITH RECOIL */
// TODO: see if Submission, DoubleEdge, TakeDown also have special substitute considerations
class Struggle extends PhysicalMove with Recoil with SingleStrike {
  // TODO: weird stuff with substitute, etc.
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
  override val type1 = Normal
  override val power = 100  // higher in later generations
  override val maxPP = 15
  override val recoilProportion = 0.25
}

class TakeDown extends PhysicalMove with Recoil with SingleStrike {
  override val index = 36
  override val type1 = Normal
  override val power = 90
  override val maxPP = 20
  override val accuracy = 0.85
  override val recoilProportion = 0.25
}


/* PHYSICAL, VANILLA SINGLESTRIKE */
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
  override val type1 = Normal
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
  override val type1 = Normal
  override val power = 70
  override val maxPP = 10
}

class EggBomb extends PhysicalMove with SingleStrike {
  override val index = 121
  override val type1 = Normal
  override val power = 100
  override val maxPP = 10
  override val accuracy = 0.75
}

class HornAttack extends PhysicalMove with SingleStrike {
  override val index = 30
  override val type1 = Normal
  override val power = 65
  override val maxPP = 25
}

class MegaKick extends PhysicalMove with SingleStrike {
  override val index = 25
  override val type1 = Normal
  override val power = 120
  override val maxPP = 5
  override val accuracy = 0.75
}

class MegaPunch extends PhysicalMove with SingleStrike {
  override val index = 5
  override val type1 = Normal
  override val power = 80
  override val maxPP = 20
  override val accuracy = 0.85
}

class PayDay extends PhysicalMove with SingleStrike {
  override val index = 6
  override val type1 = Normal
  override val power = 40
  override val maxPP = 20
}

class Scratch extends PhysicalMove with SingleStrike {
  override val index = 10
  override val type1 = Normal
  override val power = 40
  override val maxPP = 35
}

class Slam extends PhysicalMove with SingleStrike {
  override val index = 21
  override val type1 = Normal
  override val power = 80
  override val maxPP = 20
  override val accuracy = 0.75
}

class Strength extends PhysicalMove with SingleStrike {
  override val index = 70
  override val type1 = Normal
  override val power = 80
  override val maxPP = 15
}

class ViceGrip extends PhysicalMove with SingleStrike {
  override val index = 11
  override val type1 = Normal
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


/* PHYSICAL, SINGLESTRIKE, WEIRD */
class KarateChop extends PhysicalMove with SingleStrike {
  override val index = 2
  override val type1 = Normal    // Fighting in later generations
  override val power = 50
  override val maxPP = 25
  override val critHitRate = HIGH
}

class QuickAttack extends PhysicalMove with SingleStrike {
  override val index = 98
  override val type1 = Normal
  override val power = 40
  override val maxPP = 30
  override val priority = 1
}

class Slash extends PhysicalMove with SingleStrike {
  override val index = 163
  override val type1 = Normal
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



/******** SPECIAL MOVES ********/
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
