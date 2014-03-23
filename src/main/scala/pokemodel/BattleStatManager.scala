package pokemodel

import scala.collection.mutable

/*
 * BattleStatManagers keep track of the stages of attack, defense, speed,
 * special, accuracy, and evasion for each Pokemon in a battle. Each battle
 * has its own BattleStatManager instance.
 *
 * BattleStatManagers also know how to take the stats they contain and use them
 * plus a Pokemon's base stats to calculate an effective attack, defense, speed,
 * special, accuracy, and evasion
 */

class BattleStatManager (team1: PokemonTeam, team2: PokemonTeam) {
  // http://bulbapedia.bulbagarden.net/wiki/Stats#Stages
  private val init = List.fill(team1.length + team2.length)(0)
  private val allPokemon = team1.team ++ team2.team

  val attackStages:   mutable.Map[Pokemon, Int] = mutable.Map()
  val defenseStages:  mutable.Map[Pokemon, Int] = mutable.Map()
  val speedStages:    mutable.Map[Pokemon, Int] = mutable.Map()
  val specialStages:  mutable.Map[Pokemon, Int] = mutable.Map()
  val evasionStages:  mutable.Map[Pokemon, Int] = mutable.Map()
  val accuracyStages: mutable.Map[Pokemon, Int] = mutable.Map()

  for ((p, zero) <- allPokemon.zip(init)) {
    attackStages(p)   = zero
    defenseStages(p)  = zero
    speedStages(p)    = zero
    specialStages(p)  = zero
    evasionStages(p)  = zero
    accuracyStages(p) = zero
  }

  private val attackStageToFraction = Map(
      -6 -> 0.25,
      -5 -> 0.29,
      -4 -> 0.33,
      -3 -> 0.40,
      -2 -> 0.50,
      -1 -> 0.67,
      0 -> 1.0,
      1 -> 1.5,
      2 -> 2.0,
      3 -> 2.5,
      4 -> 3.0,
      5 -> 3.5,
      6 -> 4.0
  )
  private def defenseStageToFraction = attackStageToFraction
  private def specialStageToFraction = attackStageToFraction
  private def speedStageToFraction = attackStageToFraction

  private val accuracyStageToFraction = Map(
      -6 -> 0.33,
      -5 -> 0.38,
      -4 -> 0.43,
      -3 -> 0.50,
      -2 -> 0.60,
      -1 -> 0.75,
      0 -> 1.0,
      1 -> 1.33,
      2 -> 1.67,
      3 -> 2.0,
      4 -> 2.33,
      5 -> 2.66,
      6 -> 3.0
  )
  private def evasionStageToFraction = accuracyStageToFraction

  /* Ways for the Battle to interact with the stats */
  // TODO: Take status ailments into account, like the fact that BRN reduces attack by half, etc.
  def getEffectiveAttack(p: Pokemon) : Int = {
    require(attackStages.contains(p), s"getEffectiveAttack error for $p")
    (attackStageToFraction(attackStages(p)) * p.attack).toInt
  }

  def getEffectiveDefense(p: Pokemon) : Int = {
    require(defenseStages.contains(p), s"getEffectiveDefense error for $p")
    (defenseStageToFraction(defenseStages(p)) * p.defense).toInt
  }

  def getEffectiveSpecial(p: Pokemon) : Int = {
    require(specialStages.contains(p), s"getEffectiveSpecial error for $p")
    (specialStageToFraction(specialStages(p)) * p.special).toInt
  }

  def getEffectiveSpeed(p: Pokemon) : Int = {
    require(speedStages.contains(p), s"getEffectiveSpeed error for $p")
    (speedStageToFraction(speedStages(p)) * p.speed).toInt
  }

  def getEffectiveAccuracy(p: Pokemon) : Int = {
    require(accuracyStages.contains(p), s"getEffectiveAccuracy error for $p")
    accuracyStageToFraction(accuracyStages(p)).toInt
  }

  def getEffectiveEvasion(p: Pokemon) : Int = {
    require(evasionStages.contains(p), s"getEffectiveEvasion error for $p")
    evasionStageToFraction(evasionStages(p)).toInt
  }

  /* Ways to increase/decrease stages for Pokemon
   * For example, to increase a Pokemon's attack by 1, use changeAttackStage(p, 1)
   * To decrease an opponent's defense by 2, use changeDefenseStat(opponent, -2)
   * */
  private def curbNewTotal(newTotal : Int) : Int = {
    // Valid stages are between -6 and 6... moves that push a stat past that
    // have no effect
    if (newTotal > 6) 6
    else if (newTotal < -6) -6
    else newTotal
  }

  def changeAttackStage(p: Pokemon, change: Int) : Unit = {
    require(attackStages.contains(p), s"changeAttackStage error for $p")
    val newTotal = attackStages(p) + change
    attackStages(p) = curbNewTotal(newTotal)
  }

  def changeDefenseStage(p: Pokemon, change: Int) : Unit = {
    require(defenseStages.contains(p), s"changeDefenseStage error for $p")
    val newTotal = defenseStages(p) + change
    defenseStages(p) = curbNewTotal(newTotal)
  }

  def changeSpecialStage(p: Pokemon, change: Int) : Unit = {
    require(specialStages.contains(p), s"changeSpecialStage error for $p")
    val newTotal = specialStages(p) + change
    specialStages(p) = curbNewTotal(newTotal)
  }

  def changeSpeedStage(p: Pokemon, change: Int) : Unit = {
    require(speedStages.contains(p), s"changeSpeedStage error for $p")
    val newTotal = speedStages(p) + change
    speedStages(p) = curbNewTotal(newTotal)
  }

  def changeAccuracyStage(p: Pokemon, change: Int) : Unit = {
    require(accuracyStages.contains(p), s"changeAccuracyStage error for $p")
    val newTotal = accuracyStages(p) + change
    accuracyStages(p) = curbNewTotal(newTotal)
  }

  def changeEvasionStage(p: Pokemon, change: Int) : Unit = {
    require(evasionStages.contains(p), s"changeEvasionStage error for $p")
    val newTotal = evasionStages(p) + change
    evasionStages(p) = curbNewTotal(newTotal)
  }

  // Ways to set the stats to an absolute value
  // Useful in a few rare settings
  def setAttackStage(p: Pokemon, newValue: Int) : Unit   = { attackStages(p)   = newValue }
  def setDefenseStage(p: Pokemon, newValue: Int) : Unit  = { defenseStages(p)  = newValue }
  def setSpecialStage(p: Pokemon, newValue: Int) : Unit  = { specialStages(p)  = newValue }
  def setSpeedStage(p: Pokemon, newValue: Int) : Unit    = { speedStages(p)    = newValue }
  def setAccuracyStage(p: Pokemon, newValue: Int) : Unit = { accuracyStages(p) = newValue }
  def setEvasionStage(p: Pokemon, newValue: Int) : Unit  = { evasionStages(p)  = newValue }

  def resetAll(p: Pokemon) : Unit = {
    // Useful for Haze
    setAttackStage(p, 0)
    setDefenseStage(p, 0)
    setSpecialStage(p, 0)
    setSpeedStage(p, 0)
    setAccuracyStage(p, 0)
    setEvasionStage(p, 0)
  }
}
