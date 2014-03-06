package pokemodel

import scala.util.Random

object PokemonBuilder {
  private val minEVValue = 0
  private val maxEVValue = 65535
  private val minIVValue = 0
  private val maxIVValue = 15
  private val evSD = 3000  // SD of EV values generated

  private def generateRandomScaledEV(level : Int) : Int = {
    val mean = (level.toFloat / Pokemon.maxLevel) * maxEVValue
    val sd = evSD
    val ev = (Random.nextGaussian() * mean + sd).toInt
    if (ev > maxEVValue) maxEVValue 
      else if (ev < 0) 0 
      else ev
  }
  
  // Functions that scale up base stats to correct stats based on level, EV, IV, and base HP
  private def scaleHP(iv : Int, ev : Int, level : Int, baseHP : Int) : Int = {
    val numerator = (iv + baseHP + (Math.sqrt(ev)/8) + 50) * level
    ((numerator / 50) + 10).toInt
  }

  private def scaleOtherStat(iv : Int, baseStat : Int, ev : Int, level : Int) : Int = {
    val numerator = (iv + baseStat + (Math.sqrt(ev)/8) + 50) * level
    ((numerator / 50) + 5).toInt
  }
}

class PokemonBuilder (index : Int, level : Int){
  require(1 <= index && index <= Pokemon.numPokemon)
  require(1 <= level && level <= Pokemon.maxLevel)
  
  val indx = index
  val lvl = level

  // Alt constructor: create a PokemonBuilder by specifying Pokemon's name instead of its index
  def this(name : String, level : Int) = this(Pokemon.nameToID(name), level)

  /* Optional parameters - default to random values */
  // Pokemon have 5 individual values (IVs) that are generated when their created
  // These are one reason why two Pokemon of the same level + species can have
  // different statistics
  var attackIV  = Random.nextInt(15)
  var defenseIV = Random.nextInt(15)
  var speedIV   = Random.nextInt(15)
  var specialIV = Random.nextInt(15)
   
  // hpIV is a function of the other four IVs
  // Implement as a function so that it will be correct despite changes to other IVs
  def hpIV = List(8, 4, 2, 1).zip(
    List(attackIV, defenseIV, speedIV, specialIV)).filter(
      pair => pair._2 % 2 == 1).foldLeft(0)((x, y) => x + y._1)

  var currentHP = maxHP
  
  // Pokemon also have Effort Values (EVs) that are gained when enemies are defeated
  // I don't have a good way to know what these will be ahead of time, so I assume they
  // follow a Gaussian(p * maxEVValue, SD = evSD) distribution, then truncate to
  // ensure they're in the range 0 .. 65535 == minEVValue .. maxEVValue
  var hpEV      = PokemonBuilder.generateRandomScaledEV(level)
  var attackEV  = PokemonBuilder.generateRandomScaledEV(level)
  var defenseEV = PokemonBuilder.generateRandomScaledEV(level)
  var speedEV   = PokemonBuilder.generateRandomScaledEV(level)
  var specialEV = PokemonBuilder.generateRandomScaledEV(level)

  var move1 = null   // TODO: pick a random move this Pokemon can learn as default
  var move2 = null   // TODO: pick a random move this Pokemon can learn as default
  var move3 = null   // TODO: pick a random move this Pokemon can learn as default
  var move4 = null   // TODO: pick a random move this Pokemon can learn as default
  
  // Those are actually the only values that the user can input...
  // Stats are determined by the level and that species's base stats
  // Types are determined exclusively by index
  // Determine this stuff below
  def name    = Pokemon.idToName(index)
  def maxHP   = PokemonBuilder.scaleHP(hpIV, Pokemon.getBaseHP(index), hpEV, level)
  def attack  = PokemonBuilder.scaleOtherStat(attackIV, Pokemon.getBaseAttack(index), attackEV, level)
  def defense = PokemonBuilder.scaleOtherStat(defenseIV, Pokemon.getBaseDefense(index), defenseEV, level)
  def speed   = PokemonBuilder.scaleOtherStat(speedIV, Pokemon.getBaseSpeed(index), speedEV, level)
  def special = PokemonBuilder.scaleOtherStat(specialIV, Pokemon.getBaseSpecial(index), specialEV, level)
  def type1   = Pokemon.getType1(index);
  def type2   = Pokemon.getType2(index);
  
  /* Methods to change changeable values while building */
  def attackIV(value : Int) : PokemonBuilder = { 
    require(PokemonBuilder.minIVValue <= value && value <= PokemonBuilder.maxIVValue) 
    attackIV = value
    this
  }

  def defenseIV(value : Int) : PokemonBuilder = {
    require(PokemonBuilder.minIVValue <= value && value <= PokemonBuilder.maxIVValue)
    defenseIV = value
    this
  }

  def speedIV(value : Int) : PokemonBuilder = {
    require(PokemonBuilder.minIVValue <= value && value <= PokemonBuilder.maxIVValue)
    speedIV = value
    this
  }

  def specialIV(value : Int) : PokemonBuilder = {
    require(PokemonBuilder.minIVValue <= value && value <= PokemonBuilder.maxIVValue)
    specialIV = value
    this
  }

  def hpEV(value : Int) : PokemonBuilder = {
    require(PokemonBuilder.minEVValue <= value && value <= PokemonBuilder.maxEVValue)
    hpEV = value
    this
  }

  def attackEV(value : Int) : PokemonBuilder = {
    require(PokemonBuilder.minEVValue <= value && value <= PokemonBuilder.maxEVValue)
    attackEV = value
    this
  }

  def defenseEV(value : Int) : PokemonBuilder = {
    require(PokemonBuilder.minEVValue <= value && value <= PokemonBuilder.maxEVValue)
    defenseEV = value
    this
  }

  def speedEV(value : Int) : PokemonBuilder = {
    require(PokemonBuilder.minEVValue <= value && value <= PokemonBuilder.maxEVValue)
    speedEV = value
    this
  }

  def specialEV(value : Int) : PokemonBuilder = {
    require(PokemonBuilder.minEVValue <= value && value <= PokemonBuilder.maxEVValue)
    specialEV = value
    this
  }
  
  def currentHP(value : Int) : PokemonBuilder = {
    require(0 <= value && value <= maxHP)
    currentHP = value
    this
  }
  
  def maxOut() : PokemonBuilder = {
    attackIV(PokemonBuilder.maxIVValue)
    defenseIV(PokemonBuilder.maxIVValue)
    speedIV(PokemonBuilder.maxIVValue)
    specialIV(PokemonBuilder.maxIVValue)
    hpEV(PokemonBuilder.maxEVValue)
    attackEV(PokemonBuilder.maxEVValue)
    defenseEV(PokemonBuilder.maxEVValue)
    speedEV(PokemonBuilder.maxEVValue)
    specialEV(PokemonBuilder.maxEVValue)
    this
  }
}
