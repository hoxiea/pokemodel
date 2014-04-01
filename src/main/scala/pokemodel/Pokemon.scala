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

class Pokemon(builder : PokemonBuilder) {
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
  var type1 = builder.type1
  var type2 = builder.type2

  // Moves can change in battle too, believe it or not
  var move1 : Option[Move] = builder.move1
  var move2 : Option[Move] = builder.move2
  var move3 : Option[Move] = builder.move3
  var move4 : Option[Move] = builder.move4

  // Every Pokemon knows how to Struggle, but can only do so if
  // they're out of PP / disabled with every other move
  val move5 : Move = new Struggle()

  // Pokemon keep track of the PP they have left for each move; start out full
  var pp1 : Option[Int] = move1.map(_.maxPP)
  var pp2 : Option[Int] = move2.map(_.maxPP)
  var pp3 : Option[Int] = move3.map(_.maxPP)
  var pp4 : Option[Int] = move4.map(_.maxPP)
  val pp5 : Option[Int] = Some(1)

  val attack  = builder.attack
  val defense = builder.defense
  val speed   = builder.speed
  val special = builder.special
  val maxHP   = builder.maxHP

  private var currHP = builder.currentHP
  var statusAilment : Option[StatusAilment] = builder.statusAilment

  // Now we add substitute stuff
  private var subHP: Option[Int] = None
  def hasSub : Boolean = subHP.isDefined
  private def resetSub() = { subHP = None }

  /* METHODS */
  def isAlive: Boolean = currentHP > 0
  def currentHP(bypassSub: Boolean = false): Int = subHP match {
    case Some(hp) => hp
    case None => currHP
  }

  def takeDamage(damage: Int, bypassSub: Boolean = false): TakeDamageResult = {
    // If you have a substitute, it should absorb damage
    require(0 <= damage && damage <= currentHP,
        "Don't expect Pokemon.takeDamage to truncate for you!")
    subHP match {
      case Some(sHP) => {
        if (sHP > damage) {
          // CASE 1: SUB HAS ENOUGH HEALTH TO SURVIVE ATTACK; ABSORB HIT
          subHP = Some(sHP - damage)
          ALIVE
        } else {
          // CASE 2: SUB BREAKS
          resetSub()
          SUBKO
        }
      }
      case None => {
        // CASE 3: No substitute, just take the hit
        currHP = currHP - damage
        if (currHP > 0) ALIVE else KO
      }
    }
  }

  def gainHP(amount : Int) { currHP = maxHP min (currHP + amount) }

  def getMove(index: Int): Option[Move] = {
    require(1 <= index && index <= 4, s"illegal index $index passed to getMove - $name($level)")
    index match {
      case 1 => move1
      case 2 => move2
      case 3 => move3
      case 4 => move4
    }
  }

  def getPP(index: Int): Option[Int] = {
    require(1 <= index && index <= 4, s"illegal index $index passed to getPP - $name($level)")
    index match {
      case 1 => pp1
      case 2 => pp2
      case 3 => pp3
      case 4 => pp4
    }
  }

  private def canUseMove(index: Int, battle: Battle): Boolean = {
    require(1 <= index && index <= 4, s"illegal index $index passed to canUseMove - $name($level)")

    // A Pokemon must have Some(Move) to be able to use it
    val moveOption = getMove(index)
    if (moveOption.isEmpty)
      throw new Exception(s"$name tried to use Move${index}, but it doesn't have a Move!")

    // It also has to have Some(pp)
    val ppOption = getPP(index)
    if (ppOption.isEmpty)
      throw new Exception(s"$name tried to use Move${index} and it has a Move${index}, but pp${index} = None!")

    // And that Some(pp) has to feature pp > 0
    if (ppOption.get <= 0)
      throw new Exception(s"$name tried to use Move${index} but pp${index} = 0!")

    // TODO: check battle for disabled
    true
  }

  def useMove(index : Int, enemy : Pokemon, battle : Battle): MoveResult = {
    require(1 <= index && index <= 5, s"illegal index $index passed to useMove - $name $level")

    index match {
      case 1 => {
        canUseMove(index, battle)
        pp1 = Some(pp1.get - 1)
        move1.get.use(this, enemy, battle)
      }
      case 2 => {
        canUseMove(index, battle)
        pp2 = Some(pp2.get - 1)
        move2.get.use(this, enemy, battle)
      }
      case 3 => {
        canUseMove(index, battle)
        pp3 = Some(pp3.get - 1)
        move3.get.use(this, enemy, battle)
      }
      case 4 => {
        canUseMove(index, battle)
        pp4 = Some(pp4.get - 1)
        move4.get.use(this, enemy, battle)
      }
      case 5 => move5.use(this, enemy, battle)  // can always use, don't deduct PP
    }
  }

  def takeStatusAilmentDamage() : Unit = statusAilment match {
    case Some(_ : PSN) => takeDamage(maxHP / 16)
    case Some(_ : BRN) => takeDamage(maxHP / 16)
    case _ => {}
  }

  def isBurned: Boolean = statusAilment match {
    case Some(_ : BRN) => true
    case _ => false
  }

  def isFrozen: Boolean = statusAilment match {
    case Some(_ : FRZ) => true
    case _ => false
  }

  def isPoisoned: Boolean = statusAilment match {
    case Some(_ : PSN) => true
    case _ => false
  }

  def isBadlyPoisoned: Boolean = statusAilment match {
    case Some(_ : BPSN) => true
    case _ => false
  }

  def isParalyzed: Boolean = statusAilment match {
    case Some(_ : PAR) => true
    case _ => false
  }

  def isAsleep: Boolean = statusAilment match {
    case Some(_ : SLP) => true
    case _ => false
  }

  /* NICETIES */
  private def checkConsistency {
    assert(1 <= index && index <= 165)
    assert(0 <= currentHP && currentHP <= maxHP)

    // Check that moves that exist have existing PPs, and that non-existant moves don't


  }
  private def allInfoString : String = {
    val repr = new StringBuilder()
    repr.append(s"$name, level $level\n")
    repr.append(s"Type1 = $type1, Type2 = $type2\n")
    repr.append(s"HP = $currentHP / $maxHP, Status = $statusAilment\n")
    repr.append(s"A|D|Spd|Spcl = $attack $defense $speed $special\n")
    repr.append(s"IV (A|D|Spd|Spcl|HP) = $attackIV $defenseIV $speedIV $specialIV $hpIV\n")
    repr.append(s"EV (A|D|Spd|Spcl|HP) = $attackEV $defenseEV $speedEV $specialEV $hpEV\n")
    repr.append(s"Moves: $move1, $move2, $move3, $move4")
    repr.toString()
  }

  private def basicInfoString : String = {
    val repr = new StringBuilder()
    repr.append(s"$name, level $level\n")
    repr.append(s"HP = $currentHP / $maxHP, Status = $statusAilment\n")
    repr.toString()
  }

  override def toString : String = {
    allInfoString
  }
}
