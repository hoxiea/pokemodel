package pokemodel

import scala.io.Source
import java.io.File

object PokeData {
  val numPokemon = 151

  // Read in base stats and make useful data structures
  var baseStats : Map[Int, (String, Int, Int, Int, Int, Int)] = Map()
  var types : Map[Int, (String, String)] = Map()
  var idToName : Map[Int, String] = Map()
  var nameToID : Map[String, Int] = Map()

  val baseStatsPath = "/Users/hha/Dropbox/pokemodel/game_data/base_stats_types.csv"
  val baseStatsFile = new File(baseStatsPath)
  for (line <- Source.fromFile(baseStatsFile).getLines
       if !line.startsWith("Number")) {
    val data = line.split(", ")
    val Array(idS, name, hpS, attackS, defenseS, speedS, specialS, type1S, type2S) = data
    val Array(id, hp, attack, defense, speed, special) = Array(idS, hpS, attackS, defenseS, speedS, specialS).map(_.toInt)
    idToName += (id -> name)
    nameToID += (name -> id)
    baseStats += (id -> (name, hp, attack, defense, speed, special))
    types += (id -> (type1S, type2S))
  }

  assert(baseStats.size == PokemonBuilder.numPokemon)
  assert(idToName.size == PokemonBuilder.numPokemon)
  assert(nameToID.size == PokemonBuilder.numPokemon)
  assert(types.size == PokemonBuilder.numPokemon)

  // Methods so that other parts of the program can query this stuff
  def getBaseHP(index : Int) : Int = {
    require(1 <= index && index <= 151, s"invalid call $index to getBaseHP")
    baseStats(index)._2
  }

  def getBaseAttack(index : Int) : Int = {
    require(1 <= index && index <= numPokemon, s"invalid call $index to getBaseAttack")
    baseStats(index)._3    
  }

  def getBaseDefense(index : Int) : Int = {
    require(1 <= index && index <= numPokemon, s"invalid call $index to getBaseDefense")
    baseStats(index)._4    
  }

  def getBaseSpeed(index : Int) : Int = {
    require(1 <= index && index <= numPokemon, s"invalid call $index to getBaseSpeed")
    baseStats(index)._5    
  }

  def getBaseSpecial(index : Int) : Int = {
    require(1 <= index && index <= numPokemon, s"invalid call $index to getBaseSpecial")
    baseStats(index)._6    
  }

  def getType1(index : Int) : Type.Value = {
    val typeStrings = types(index)
    Type.stringToType(typeStrings._1)
  }

  def getType2(index : Int) : Type.Value = {
    val typeStrings = types(index)
    Type.stringToType(typeStrings._2)
  }
}
