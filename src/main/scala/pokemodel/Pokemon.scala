package pokemodel

import scala.Array.canBuildFrom
import scala.io.Source
import Type._

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

  val attack  = builder.attack
  val defense = builder.defense
  val speed   = builder.speed
  val special = builder.special
  val maxHP   = builder.maxHP

  var currentHP = maxHP
  var statusAilment : Option[StatusAilment] = builder.statusAilment


  /* METHODS */
  def isAlive: Boolean = currentHP > 0

  def takeDamage(damage : Int) {
    currentHP = if (damage >= currentHP) 0 else currentHP - damage
  }
  
  def tryToChangeStatusAilment(newStatus : StatusAilment) : Unit = statusAilment match {
    case None => { statusAilment = Some(newStatus) }
    case Some(s) => {}  // new status ailments don't overwrite old ones
  }

  def heal() {
    currentHP = maxHP
    statusAilment = None
    for (m <- List(move1, move2, move3, move4)) {
      m match {
        case Some(move) => move.restorePP
        case None => {}
      }
    }
  }
  
  def gainHP(amount : Int) {
    currentHP = intWrapper(maxHP).min(currentHP + amount)
  }

  // TODO: take Disable into account?
  // TODO: take no PP into account?
  def useMove(index : Int, enemy : Pokemon, battle : Battle) : Unit = {
    require(1 <= index && index <= 5, s"illegal index $index passed to useMove - $name $level")
    index match {
      case 1 => move1 match {
        case None => {}
        case Some(m) => m.use(this, enemy, battle)
      }
      case 2 => move2 match {
        case None => {}
        case Some(m) => m.use(this, enemy, battle)
      }
      case 3 => move3 match {
        case None => {}
        case Some(m) => m.use(this, enemy, battle)
      }
      case 4 => move4 match {
        case None => {}
        case Some(m) => m.use(this, enemy, battle)
      }
      case 5 => move5.use(this, enemy, battle)
    }
  }

  def takeStatusAilmentDamage() : Unit = statusAilment match {
    case Some(_ : PSN) => takeDamage(maxHP / 16)
    case Some(_ : BRN) => takeDamage(maxHP / 16)
    case _ => {}
  }

  /* NICETIES */
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
