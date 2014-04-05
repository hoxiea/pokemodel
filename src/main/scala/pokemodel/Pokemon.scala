package pokemodel

import scala.Array.canBuildFrom
import scala.io.Source
import Type._
import TakeDamageResult._

/*
 * TODO: Overview of the Pokemon class.
 *
 * The only complicated thing about a Pokemon is that, by using the move
 * Substitute, it can create a substitute, a sort of punching bag that absorbs
 * damage until it "breaks."
 */

class Pokemon(builder: PokemonBuilder) {
  private val attackIV  = builder.attackIV
  private val defenseIV = builder.defenseIV
  private val speedIV   = builder.speedIV
  private val specialIV = builder.specialIV
  private val hpIV      = builder.hpIV

  private val hpEV      = builder.hpEV
  private val attackEV  = builder.attackEV
  private val defenseEV = builder.defenseEV
  private val speedEV   = builder.speedEV
  private val specialEV = builder.specialEV

  val index = builder.index
  val name  = builder.name
  val level = builder.level

  // These can technically change with some really strange moves (Conversion)
  // There are methods for interacting with them below to ensure safety...
  // they really shouldn't change very often
  var type1 = builder.type1
  var type2 = builder.type2

  // Moves can change in battle too, believe it or not
  private var move1: Option[Move] = builder.move1
  private var move2: Option[Move] = builder.move2
  private var move3: Option[Move] = builder.move3
  private var move4: Option[Move] = builder.move4

  // Every Pokemon knows how to Struggle, but can only do so if
  // they're out of PP / disabled with every other move
  val move5: Move = MoveDepot(165)

  // Pokemon keep track of the PP they have left for each move; start out full
  var pp1: Option[Int] = move1.map(_.maxPP)
  var pp2: Option[Int] = move2.map(_.maxPP)
  var pp3: Option[Int] = move3.map(_.maxPP)
  var pp4: Option[Int] = move4.map(_.maxPP)
  val pp5: Option[Int] = Some(1)

  val attack  = builder.attack
  val defense = builder.defense
  val speed   = builder.speed
  val special = builder.special
  val maxHP   = builder.maxHP

  private var currHP = builder.currentHP
  var statusAilment: Option[StatusAilment] = builder.statusAilment


  /*
   * SUBSTITUTE
   * Using the move Substitute, a Pokemon can pour some of its own HP into
   * creating a punching bag-esque thing called a Substitute. See Substitute
   * in ActualMoves.scala for more information and details.
   *
   * What concerns us here is the implementation of Substitute. I basically
   * strapped an extra Option[Int] to Pokemon, called subHP, with
   * None    -> no substitute created
   * Some(i) -> the substitute has i health
   *
   * That's fine, but it complicated other things.
   * - currentHP used to be a member var; now it's a function that returns the
   *   HP of the sub if it exists, the HP of the underlying Pokemon if a sub
   *   doesn't exist, and can bypass the existence of a sub if it needs to
   * - takeDamage is equally complicated, and allows you to hurt either the
   *   underlying Pokemon if there's no sub or if you want to bypass the sub,
   *   or to hurt the sub if it exists
   */
  private var subHP: Option[Int] = None
  private def resetSub() = { subHP = None }
  def hasSub: Boolean = subHP.isDefined

  def canMakeSub: Boolean = {
    if (subHP.isDefined) false                  // a sub already exists
    else if (currentHP() < maxHP / 4) false     // not enough HP
    else if (currentHP() == maxHP / 4) true     // you can, but it'll KO you
    else true
  }

  def makeSub() = {
    if (!canMakeSub)
      throw new Exception("Check with canMakeSub before trying to make one")
    val hpToLose =
      if (maxHP <= 3) 0
      else maxHP / 4

    // If you have exactly maxHP/4 health, making a sub will KO you
    // Check for this, and just process the faint if it happens
    if (currentHP() == hpToLose) faint()
    else {
      val hpSubGains = hpToLose + 1
      subHP = Some(hpSubGains)
      currHP -= hpToLose
      assert (currHP > 0)
    }
  }


  /* GENERAL POKEMON METHODS */
  def isAlive: Boolean = currHP > 0
  def currentHP(bypassSub: Boolean = false): Int = {
    if (bypassSub) currHP
    else subHP match {
      case Some(hp) => hp
      case None => currHP
    }
  }

  def faint() = {}   // TODO: implement Pokemon.faint

  def takeDamage(damage: Int, bypassSub: Boolean = false): TakeDamageResult = {
    // If you have a substitute, it should absorb damage
    require(0 <= damage && damage <= currentHP(bypassSub),
        "Don't expect Pokemon.takeDamage to truncate for you!")

    if (bypassSub) {
      // CASE 1: No substitute, just take the hit
      currHP = currHP - damage
      if (currHP > 0) ALIVE else KO
    } else subHP match {
      case Some(sHP) => {
        if (sHP > damage) {
          // CASE 2: SUB HAS ENOUGH HEALTH TO SURVIVE ATTACK; ABSORB HIT
          subHP = Some(sHP - damage)
          ALIVE
        } else {
          // CASE 3: SUB BREAKS
          resetSub()
          SUBKO
        }
      }
      case None => {
        // CASE 1 again: No substitute, just take the hit
        currHP = currHP - damage
        if (currHP > 0) ALIVE else KO
      }
    }
  }

  def gainHP(amount: Int) { currHP = maxHP min (currHP + amount) }
  def toFullHealth() = { currHP = maxHP }

  def heal() {
    toFullHealth()
    removeStatusAilment()
    subHP = None
    // RHS same code as pp initialization code
    if (move1.isDefined) pp1 = move1.map(_.maxPP)
    if (move2.isDefined) pp2 = move2.map(_.maxPP)
    if (move3.isDefined) pp3 = move3.map(_.maxPP)
    if (move4.isDefined) pp4 = move4.map(_.maxPP)
  }

  /*
   * TYPES
   * A Pokemon's type(s) seems like it should be immutable, but unfortunately,
   * Porygon had to come along and mess that up. The move Conversion changes
   * Porygon's types to be those of his opponent. But this is the only way to
   * change types, so I add setters that can ensure that Conversion is the only
   * thing trying to make this happen.
   */
  def changeType1(newType: Type, m: Move) {
    if (m.index != 160) // 160 => Conversion
      throw new Exception("something other than Conversion changing type1")
    type1 = newType
  }

  def changeType2(newType: Type, m: Move) {
    if (m.index != 160) // 160 => Conversion
      throw new Exception("something other than Conversion changing type2")
    type2 = newType
  }

  def resetTypes() {
    type1 = PokeData.getType1(index)
    type2 = PokeData.getType2(index)
  }

  /*
   * MOVES
   * Pokemon moves are implemented as Option[Int]s, where the Int refers to the
   * move index of the Move, between 1 and 165. See MoveMaker for the mapping.
   */

  def getMove(index: Int): Option[Move] = {
    require(1 <= index && index <= 4,
      s"illegal index $index passed to getMove - $name($level)")
    index match {
      case 1 => move1
      case 2 => move2
      case 3 => move3
      case 4 => move4
    }
  }

  def getPP(index: Int): Option[Int] = {
    require(1 <= index && index <= 4,
      s"illegal index $index passed to getPP - $name($level)")
    index match {
      case 1 => pp1
      case 2 => pp2
      case 3 => pp3
      case 4 => pp4
    }
  }

  def deductPP(index: Int) = {
    require(1 <= index && index <= 5,
      s"illegal index $index passed to deductPP - $name($level)")
    index match {
      case 1 => pp1 = pp1.map(pp => pp - 1)
      case 2 => pp2 = pp2.map(pp => pp - 1)
      case 3 => pp3 = pp3.map(pp => pp - 1)
      case 4 => pp4 = pp4.map(pp => pp - 1)
      case 5 => {}
    }
  }

  private def canUseMove(index: Int, battle: Battle): Boolean = {
    require(1 <= index && index <= 4, s"illegal index $index passed to canUseMove - $name($level)")

    // A Pokemon must have Some(Move) to be able to use it
    val moveOption = getMove(index)
    if (moveOption.isEmpty) {
      println("canUseMove, fail 1")
      false
    }

    // It also has to have Some(pp)
    val ppOption = getPP(index)
    if (ppOption.isEmpty) {
      println("canUseMove, fail 2")
      false
    }

    // And that Some(pp) has to feature pp > 0
    if (ppOption.get <= 0) {
      println("canUseMove, fail 3")
      false
    }

    // TODO: check battle for disabled
    true
  }

  def useMove(index: Int, enemy: Pokemon, battle: Battle): MoveResult = {
    require(1 <= index && index <= 5, s"illegal index $index passed to useMove - $name $level")

    index match {
      case 5 => move5.use(this, 5, enemy, battle)  // can always use
      case i => {
        if (canUseMove(i, battle)) {
          getMove(i).get.use(this, i, enemy, battle)
        } else throw new Exception(s"Tried to use Move${i}, but can't!")
      }
    }
  }

  /*
   * STATUS AILMENT LOGIC
   */
  def removeStatusAilment() = { statusAilment = None }
  def takeStatusAilmentDamage(): Unit = statusAilment match {
    case Some(_: PSN) => takeDamage(maxHP / 16)
    case Some(_: BRN) => takeDamage(maxHP / 16)
    case _ => {}
  }

  def isBurned: Boolean = statusAilment match {
    case Some(_: BRN) => true
    case _ => false
  }

  def isFrozen: Boolean = statusAilment match {
    case Some(_: FRZ) => true
    case _ => false
  }

  def isPoisoned: Boolean = statusAilment match {
    case Some(_: PSN) => true
    case _ => false
  }

  def isBadlyPoisoned: Boolean = statusAilment match {
    case Some(_: BPSN) => true
    case _ => false
  }

  def isParalyzed: Boolean = statusAilment match {
    case Some(_: PAR) => true
    case _ => false
  }

  def isAsleep: Boolean = statusAilment match {
    case Some(_: SLP) => true
    case _ => false
  }

  /* NICETIES */
  private def checkConsistency {
    assert(1 <= index && index <= 165)
    assert(0 <= currHP && currHP <= maxHP)

    // Check that moves that exist have existing PPs, and that non-existant moves don't
    if (pp1.isDefined) assert(move1.isDefined)
    if (pp2.isDefined) assert(move2.isDefined)
    if (pp3.isDefined) assert(move3.isDefined)
    if (pp4.isDefined) assert(move4.isDefined)
    if (move1.isDefined) assert(pp1.isDefined)
    if (move2.isDefined) assert(pp2.isDefined)
    if (move3.isDefined) assert(pp3.isDefined)
    if (move4.isDefined) assert(pp4.isDefined)
  }

  private def allInfoString: String = {
    val repr = new StringBuilder()
    repr.append(s"$name, level $level\n")
    repr.append(s"Type1 = $type1, Type2 = $type2\n")
    repr.append(s"HP = $currHP / $maxHP, Status = $statusAilment\n")
    repr.append(s"A|D|Spd|Spcl = $attack $defense $speed $special\n")
    repr.append(s"IV (A|D|Spd|Spcl|HP) = $attackIV $defenseIV $speedIV $specialIV $hpIV\n")
    repr.append(s"EV (A|D|Spd|Spcl|HP) = $attackEV $defenseEV $speedEV $specialEV $hpEV\n")
    repr.append(s"Moves: $move1, $move2, $move3, $move4")
    repr.toString()
  }

  private def basicInfoString: String = {
    val repr = new StringBuilder()
    repr.append(s"$name, level $level\n")
    if (hasSub) repr.append(s"subHP = ${currentHP()}\n")
    repr.append(s"HP = $currHP / $maxHP, Status = $statusAilment\n")
    repr.toString()
  }

  override def toString: String = {
    allInfoString
  }
}
