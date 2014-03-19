package pokemodel

import scala.util.Random

class PokemonBuilder (index : Int, level : Int){
  require(1 <= index && index <= PokemonBuilder.numPokemon,
      s"invalid index $index passed to PokemonBuilder constructor")
  require(1 <= level && level <= PokemonBuilder.maxLevel,
      s"invalid level $level passed to PokemonBuilder constructor")

  val indx = index
  val lvl = level

  // Alt. constructor: create a PokemonBuilder by specifying Pokemon's name instead of its index
  // TODO: This will crash if a name is misspelled; make more resilient?
  def this(name : String, level : Int) = this(PokeData.nameToID(name), level)

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

  // Pokemon also have Effort Values (EVs) that are gained when enemies are defeated
  // I don't have a good way to know what these will be ahead of time, so I assume they
  // follow a Gaussian(p * maxEVValue, SD = evSD) distribution, then truncate to
  // ensure they're in the range 0 .. 65535 == minEVValue .. maxEVValue
  var hpEV      = PokemonBuilder.generateRandomScaledEV(level)
  var attackEV  = PokemonBuilder.generateRandomScaledEV(level)
  var defenseEV = PokemonBuilder.generateRandomScaledEV(level)
  var speedEV   = PokemonBuilder.generateRandomScaledEV(level)
  var specialEV = PokemonBuilder.generateRandomScaledEV(level)

  var move1: Option[Move] = None // TODO: pick a random move this Pokemon can learn as default
  var move2: Option[Move] = None // TODO: pick a random move this Pokemon can learn as default
  var move3: Option[Move] = None // TODO: pick a random move this Pokemon can learn as default
  var move4: Option[Move] = None // TODO: pick a random move this Pokemon can learn as default

  var statusAilment : Option[StatusAilment.Value] = None

  /* Those are actually the only values that the user can input...
   * Everything else is a function of the above values
   * Define and determine those below... mostly define, so that builder changes
   * are propagated to the resulting Pokemon
   */
  private def scaleHP(iv : Int, ev : Int, level : Int, baseHP : Int) : Int = {
    val numerator = (iv + baseHP + (Math.sqrt(ev)/8) + 50) * level
    ((numerator / 50) + 10).toInt
  }

  private def scaleOtherStat(iv : Int, ev : Int, level : Int, baseStat : Int) : Int = {
    val numerator = (iv + baseStat + (Math.sqrt(ev)/8)) * level
    ((numerator / 50) + 5).toInt
  }

  def name    = PokeData.idToName(index)
  def maxHP   = scaleHP(hpIV, hpEV, level, PokeData.getBaseHP(index))
  def attack  = scaleOtherStat(attackIV,  attackEV,  level, PokeData.getBaseAttack(index))
  def defense = scaleOtherStat(defenseIV, defenseEV, level, PokeData.getBaseDefense(index))
  def speed   = scaleOtherStat(speedIV,   speedEV,   level, PokeData.getBaseSpeed(index))
  def special = scaleOtherStat(specialIV, specialEV, level, PokeData.getBaseSpecial(index))
  def type1   = PokeData.getType1(index)
  def type2   = PokeData.getType2(index)

  var currentHP = maxHP

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

  def statusAilment(value : StatusAilment.Value) : PokemonBuilder = {
    statusAilment = Some(value)
    this
  }

  def move1(m : Move) : PokemonBuilder = {
    // TODO: make sure that this Pokemon can learn m; ignore call if it can't learn m
    move1 = Some(m)
    this
  }

  def move2(m : Move) : PokemonBuilder = {
    // TODO: make sure that this Pokemon can learn m; ignore call if it can't learn m
    move2 = Some(m)
    this
  }

  def move3(m : Move) : PokemonBuilder = {
    // TODO: make sure that this Pokemon can learn m; ignore call if it can't learn m
    move3 = Some(m)
    this
  }

  def move4(m : Move) : PokemonBuilder = {
    // TODO: make sure that this Pokemon can learn m; ignore call if it can't learn m
    move4 = Some(m)
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

object PokemonBuilder {
  val numPokemon = 151
  val minLevel = 1
  val maxLevel = 100
  val minEVValue = 0
  val maxEVValue = 65535
  val minIVValue = 0
  val maxIVValue = 15
  val evSD = 3000  // SD of EV values generated

  def generateRandomScaledEV(level : Int) : Int = {
    val mean = (level.toFloat / maxLevel) * maxEVValue
    val sd = evSD
    val ev = (Random.nextGaussian * sd + mean).toInt
    if (ev > maxEVValue) maxEVValue
      else if (ev < 0) 0
      else ev
  }

  def generateRandomPokemonBuilder() : PokemonBuilder = {
    val index = Utils.intBetween(1, numPokemon + 1)  // upper end exclusive
    val level = Utils.intBetween(minLevel + 49, maxLevel + 1)  // between 50 and 100
    new PokemonBuilder(index, level)
  }
}
