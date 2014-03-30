package pokemodel

import scala.util.Random

class PokemonBuilder (val index: Int, val level: Int) {
  require(1 <= index && index <= PokemonBuilder.numPokemon,
      s"invalid index $index passed to PokemonBuilder constructor")
  require(1 <= level && level <= PokemonBuilder.maxLevel,
      s"invalid level $level passed to PokemonBuilder constructor")

  // Alt. constructor: create a PokemonBuilder by specifying Pokemon's name instead of its index
  // TODO: This will crash if a name is misspelled; make more resilient?
  def this(name : String, level : Int) = this(PokeData.nameToID(name), level)

  /* Parameters that can be set - default to random values */
  // Pokemon have 5 individual values (IVs) that are generated when their created
  // These are one reason why two Pokemon of the same level + species can have
  // different statistics
  var attackIV  = Random.nextInt(15)
  var defenseIV = Random.nextInt(15)
  var speedIV   = Random.nextInt(15)
  var specialIV = Random.nextInt(15)

  // hpIV is a function of the other four IVs
  // Implemented as a function so that it will be correct despite changes to other IVs
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

  var move1: Option[Move] = None
  var move2: Option[Move] = None
  var move3: Option[Move] = None
  var move4: Option[Move] = None

  var statusAilment : Option[StatusAilment] = None

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

  // currentHP is a bit of a mess. It needs to be a var (instead of a val, or ideally a def),
  // since it changes. But this next line just uses the maxHP value that's relevant when the
  // Builder is first created and NOT after making modifications to HPIV or HPEV.
  // But of course, HPIV is a function of the other four IVs and can't be modified itself,
  // so we have to update currentHP in HPEV and the other four IV setters
  var currentHP = maxHP

  /* Methods to change changeable values while building */
  def attackIV(value : Int) : PokemonBuilder = {
    require(PokemonBuilder.minIVValue <= value && value <= PokemonBuilder.maxIVValue)
    attackIV = value
    currentHP = maxHP  // adjust
    this
  }

  def defenseIV(value : Int) : PokemonBuilder = {
    require(PokemonBuilder.minIVValue <= value && value <= PokemonBuilder.maxIVValue)
    defenseIV = value
    currentHP = maxHP  // adjust
    this
  }

  def speedIV(value : Int) : PokemonBuilder = {
    require(PokemonBuilder.minIVValue <= value && value <= PokemonBuilder.maxIVValue)
    speedIV = value
    currentHP = maxHP  // adjust
    this
  }

  def specialIV(value : Int) : PokemonBuilder = {
    require(PokemonBuilder.minIVValue <= value && value <= PokemonBuilder.maxIVValue)
    specialIV = value
    currentHP = maxHP  // adjust
    this
  }

  def hpEV(value : Int) : PokemonBuilder = {
    require(PokemonBuilder.minEVValue <= value && value <= PokemonBuilder.maxEVValue)
    hpEV = value
    currentHP = maxHP  // adjust
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

  def statusAilment(value : StatusAilment) : PokemonBuilder = {
    statusAilment = Some(value)
    this
  }

  def move(moveIndex : Int, m : Move) : PokemonBuilder = {
    require(1 <= moveIndex && moveIndex <= 4, "must have 1 <= moveIndex <= 4")
    require(LearnsetData.learnsets(index).contains(m.index) ||
            m.toString.startsWith("Test") ||
            m.toString.contains("$$") ||   // mixing in traits seems to change the name
            m.index == 165,              // Struggle, useful for testing Struggle
            s"$name can't learn $m")
    moveIndex match {
      case 1 => move1 = Some(m)
      case 2 => move2 = Some(m)
      case 3 => move3 = Some(m)
      case 4 => move4 = Some(m)
    }
    this
  }

  def maxOut() : PokemonBuilder = {
    // Push all variable stats to their max
    // Not only do competitive players do this, but having the ability
    // to do so makes it very easy to create a Pokemon of a given
    // species/level with exactly the same stats every time.
    // Very useful for testing, since then you can do damage calculations
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

  def addRandomMoves() : PokemonBuilder = {
    val moveIndices = LearnsetData.getFourRandomMoveIndices(index)
    var pb = new PokemonBuilder(index, level)
    for ((moveIndex, i) <- moveIndices.view.zipWithIndex) {
      pb.move(i+1, MoveMaker.makeMove(moveIndex))
    }
    pb
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

  def generateRandomPokemonBuilder(level: Int) : PokemonBuilder = {
    val index = Utils.intBetween(1, numPokemon + 1)  // upper end exclusive
    new PokemonBuilder(index, level)
  }
}
