package pokemodel 

import scala.Array.canBuildFrom
import scala.io.Source

object Pokemon extends App {
  val numPokemon = 151
  val minLevel = 1
  val maxLevel = 100

  // Read in base stats and make useful data structures
  var baseStats : Map[Int, (String, Int, Int, Int, Int, Int)] = Map()
  var types : Map[Int, (String, String)] = Map()
  var idToName : Map[Int, String] = Map()
  var nameToID : Map[String, Int] = Map()
  val baseStatsPath = "/Users/hha/Dropbox/pokemodel/game_data/base_stats_types.csv"
  for (line <- Source.fromFile(baseStatsPath).getLines
       if !line.startsWith("Number")) {
    val data = line.split(", ")
    val Array(idS, name, hpS, attackS, defenseS, speedS, specialS, type1S, type2S) = data
    val Array(id, hp, attack, defense, speed, special) = Array(idS, hpS, attackS, defenseS, speedS, specialS).map(_.toInt)
    idToName += (id -> name)
    nameToID += (name -> id)
    baseStats += (id -> (name, hp, attack, defense, speed, special))
    types += (id -> (type1S, type2S))
  }
  assert(baseStats.size == numPokemon)
  assert(idToName.size == numPokemon)
  assert(nameToID.size == numPokemon)
  assert(types.size == numPokemon)
  println(baseStats.size)

  def getBaseHP(index : Int) : Int = {
    require(1 <= index && index <= numPokemon, "invalid call to getBaseHP")
    this.
    baseStats(index)._2
  }

  def getBaseAttack(index : Int) : Int = {
    require(1 <= index && index <= numPokemon, "invalid call to getBaseAttack")
    baseStats(index)._3    
  }

  def getBaseDefense(index : Int) : Int = {
    require(1 <= index && index <= numPokemon, "invalid call to getBaseDefense")
    baseStats(index)._4    
  }

  def getBaseSpeed(index : Int) : Int = {
    require(1 <= index && index <= numPokemon, "invalid call to getBaseSpeed")
    baseStats(index)._5    
  }

  def getBaseSpecial(index : Int) : Int = {
    require(1 <= index && index <= numPokemon, "invalid call to getBaseSpecial")
    baseStats(index)._6    
  }

  def getType1(index : Int) : Type.Value = {
    val typeStrings = types(index)
    Type.stringToValue(typeStrings._1)
  }

  def getType2(index : Int) : Type.Value = {
    val typeStrings = types(index)
    Type.stringToValue(typeStrings._2)
  }
}


class Pokemon(val builder : PokemonBuilder) {
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

  val type1 = builder.type1
  val type2 = builder.type2

  private val move1 = builder.move1
  private val move2 = builder.move2
  private val move3 = builder.move3
  private val move4 = builder.move4

  // These can change in battle, believe it or not
  private var attack  = builder.attack
  private var defense = builder.defense
  private var speed   = builder.speed
  private var special = builder.special
  private var maxHP   = builder.maxHP

  // http://www.serebii.net/games/stats.shtml
  val evasionStage = 0    
  val accuracyStage = 0

  var currentHP = maxHP
  // var statusAilment : Status

  override def toString : String = {
    val repr = new StringBuilder()
    repr.append(s"$name, level $level")
    repr.append(s"Type1 = $type1, Type2 = $type2")
    repr.append(s"IV (A|D|Spd|Spcl|HP) = $attackIV $defenseIV $speedIV $specialIV $hpIV")
    repr.append(s"EV (A|D|Spd|Spcl|HP) = $attackEV $defenseEV $speedEV $specialEV $hpEV")
    repr.append(s"Moves: $move1, $move2, $move3, $move4")
    repr.toString()
  }

  def takeDamage(damage : Int) {
    currentHP = if (damage >= currentHP) 0 else currentHP - damage
  }

  def heal() {
    currentHP = maxHP
    // TODO : List(move1, move2, move3, move4).map(m => m.restorePP())
  }
}
