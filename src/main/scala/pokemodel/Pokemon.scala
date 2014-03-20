package pokemodel

import scala.Array.canBuildFrom
import scala.io.Source
import Type._
import StatusAilment._

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

  // These can technically change with some really strange moves (Porygon, Ditto, etc.),
  // but I'll implement that in the battle
  val type1 = builder.type1
  val type2 = builder.type2

  // Moves can change in battle too, believe it or not
  val move1 : Move = builder.move1 match {
    case Some(m: Move) => { m }
    case None => new NoMove(Some(this))
  }

  var move2 : Move = builder.move2 match {
    case Some(m: Move) => m
    case None => new NoMove(Some(this))
  }

  var move3 : Move = builder.move3 match {
    case Some(m: Move) => m
    case None => new NoMove(Some(this))
  }

  var move4 : Move = builder.move4 match {
    case Some(m: Move) => m
    case None => new NoMove(Some(this))
  }
  
  // Every Pokemon knows how to Struggle
  val move5 : Move = new Struggle(Some(this))

  val attack  = builder.attack
  val defense = builder.defense
  val speed   = builder.speed
  val special = builder.special
  val maxHP   = builder.maxHP

  var currentHP = maxHP
  var statusAilment : Option[StatusAilment.Value] = builder.statusAilment
  


  /* METHODS */
  def isAlive: Boolean = currentHP > 0
  
  def takeDamage(damage : Int) {
    currentHP = if (damage >= currentHP) 0 else currentHP - damage
  }

  def heal() {
    currentHP = maxHP
    statusAilment = None
    List(move1, move2, move3, move4).map(_.restorePP())
  }

  def getMove(index: Int) : Move = {
    require(1 <= index && index <= 4, s"illegal index $index passed to useMove - $name $level")
    index match {
      case 1 => move1
      case 2 => move2
      case 3 => move3
      case 4 => move4      
    }
  }

  def useMove(index : Int, enemy : Pokemon, battle : Battle) : Unit = {
    require(1 <= index && index <= 5, s"illegal index $index passed to useMove - $name $level")
    index match {
      case 1 => move1.use(enemy, battle)
      case 2 => move2.use(enemy, battle)
      case 3 => move3.use(enemy, battle)
      case 4 => move4.use(enemy, battle)
      case 5 => move5.use(enemy, battle)
    }
  }
  
  def takeStatusAilmentDamage() : Unit = statusAilment match {
    case PSN => takeDamage(maxHP / 16)
    case BRN => takeDamage(maxHP / 16)
    case _ => {}
  }
  
  /* NICETIES */
  private def allInfoString : String = {
    val repr = new StringBuilder()
    repr.append(s"$name, level $level\n")
    repr.append(s"Type1 = $type1, Type2 = $type2\n")
    repr.append(s"HP = $currentHP / $maxHP, Status = $statusAilment\n")
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
