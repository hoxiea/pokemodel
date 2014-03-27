package pokemodel

import scala.collection.mutable
import Battle.{verbose => VERBOSE}

/*
 * A BattleStatusManager keeps track of all of the various volatile/weird status and
 * provides useful methods for accessing this information. 
 * 
 * It also encapsulates the logic behind changing a Pokemon's single non-volatile status 
 * ailment, even though you can just change it directly via a setter.
 */

class BattleStatusManager (val team1 : PokemonTeam, val team2: PokemonTeam) {
  /*
   * Structures for tracking things that last a certain (random) number of turns
   */
  private val confusionMap = mutable.Map[Pokemon, Int]()
  private val sleepMap     = mutable.Map[Pokemon, Int]()
  private val partiallyTrappedMap = mutable.Map[Pokemon, Int]()
  
  /*
   * Structures for things that get worse with time, and need to track their progression
   */
  private val badlyPoisonedMap = mutable.Map[Pokemon, Int]()
  
  /* 
   * Structures for tracking yes/no stuff
   * A Pokemon appearing in one of these means that that status ailment is in effect
   */
  // Status Ailments
  private val flinchSet = mutable.Set[Pokemon]()
  private val seededSet = mutable.Set[Pokemon]()
  
  // Moves
  private val mistSet = mutable.Set[Pokemon]()
  private val digSet  = mutable.Set[Pokemon]()
  private val flySet  = mutable.Set[Pokemon]()
  
  /* METHODS FOR INTERACTING WITH THIS STUFF */
  def tryToCauseConfusion(p: Pokemon) {
    if (confusionMap contains p) {
      if (VERBOSE) println(s"Confusion (status) had no effect of {p.name}")
    } else {
      val confusionDuration = Utils.intBetween(BattleStatusManager.minTurnsConfusion, BattleStatusManager.maxTurnsConfusion + 1) 
      confusionMap(p) = confusionDuration
      if (VERBOSE) println(s"{p.name} will be confused for $confusionDuration turns")  // TODO: don't actually print the number
    } 
  }

  def tryToSeed(p: Pokemon) {
    if (seededSet contains p) {
      if (VERBOSE) println(s"Seeding had no effect on {p.name}")
    } else {
      seededSet += p 
      if (VERBOSE) println(s"{p.name} was seeded!")
    } 
  }

  def tryToPartiallyTrap(p: Pokemon) {
    if (partiallyTrappedMap contains p) {
      // Do nothing - can't trap again while trapped
    } else {
      partiallyTrappedMap(p) = Utils.intBetween(BattleStatusManager.minTurnsPartiallyTrapped, BattleStatusManager.maxTurnsPartiallyTrapped + 1) 
    } 
  }
  
  def causeToFlinch(p: Pokemon) {
    /*
     * Technically, only the first Pokemon to make a move during a turn can be affected by Flinch.
     * But since processTurnEnd clears flinchSet, all that happens when the second Pokemon to use a Move
     * causes a flinch is that its opponent is added to flinchSet but then is immediately cleared from it,
     * resulting in the correct fact that the opponent won't flinch on his next turn
     * So we can just use this anywhere a Flinch is needed without worrying about who attacked first
     */
    flinchSet += p
  }

  def hasMist(p : Pokemon) : Boolean = mistSet contains p

  def processSwitchOut(p : Pokemon) = {
    // TODO: take care of everything that needs to be removed, zeroed, etc. when Pokemon p switches out of battle
  }
  
  def processTurnStart() = {
    // TODO: Do everything that happens when the turn begins
    
    // Confusion seems to wear off at beginning of the turn, so decrement everyone and remove people whose Confusion cleared up
    for ((p, currentDuration) <- confusionMap) {
      if (currentDuration == 1) confusionMap -= p   
      else confusionMap(p) = currentDuration - 1
    }
  }
  
  def processTurnEnd() = {
    // TODO: Do everything that happens when the turn ends
    flinchSet.clear()  // Flinches last for exactly 1 turn
  }
  
  /*
   * Non-Volatile Status Stuff
   */
  def tryToChangeStatusAilment(p: Pokemon, newStatus : StatusAilment) = p.statusAilment match {
    case None => p.statusAilment = Some(newStatus)
    case _ => {}
  }
  

}

object BattleStatusManager {
  val minTurnsConfusion = 1
  val maxTurnsConfusion = 4
  val minTurnsPartiallyTrapped = 1
  val maxTurnsPartiallyTrapped = 4
}