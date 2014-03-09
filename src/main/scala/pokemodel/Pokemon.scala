package pokemodel 

import scala.Array.canBuildFrom
import scala.io.Source

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

  val index = builder.indx
  val name  = builder.name
  val level = builder.lvl

  var type1 = builder.type1
  var type2 = builder.type2

  // These can change in battle, believe it or not
  var move1 = builder.move1
  var move2 = builder.move2
  var move3 = builder.move3
  var move4 = builder.move4

  // These can change in battle, believe it or not
  private var attack  = builder.attack
  private var defense = builder.defense
  private var speed   = builder.speed
  private var special = builder.special
  private var maxHP   = builder.maxHP

  // http://www.serebii.net/games/stats.shtml
  // val evasionStage = 0    
  // val accuracyStage = 0
  // These will probably be a part of the Battle, not the Pokemon per se

  var currentHP = maxHP
  // var statusAilment : Status

  override def toString : String = {
    val repr = new StringBuilder()
    repr.append(s"$name, level $level\n")
    repr.append(s"Type1 = $type1, Type2 = $type2\n")
    repr.append(s"HP = $currentHP / $maxHP\n")
    repr.append(s"IV (A|D|Spd|Spcl|HP) = $attackIV $defenseIV $speedIV $specialIV $hpIV\n")
    repr.append(s"EV (A|D|Spd|Spcl|HP) = $attackEV $defenseEV $speedEV $specialEV $hpEV\n")
    repr.append(s"Moves: $move1, $move2, $move3, $move4")
    repr.toString()
  }

  def takeDamage(damage : Int) {
    currentHP = if (damage >= currentHP) 0 else currentHP - damage
  }

  def heal() {
    currentHP = maxHP
    // List(move1, move2, move3, move4).filter(_ != null).map(_.restorePP())
  }
}
