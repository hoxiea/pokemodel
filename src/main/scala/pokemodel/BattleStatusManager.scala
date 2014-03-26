package pokemodel

import scala.collection.mutable

/*
 * A BattleStatusManager keeps track of all of the various volatile/weird status and
 * provides useful methods for accessing this information.
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
  private val flinchSet = mutable.Set[Pokemon]()
  private val seededSet = mutable.Set[Pokemon]()
  private val mistSet   = mutable.Set[Pokemon]()
  
  def hasMist(p : Pokemon) : Boolean = mistSet contains p
  
  /* METHODS FOR INTERACTING WITH THIS STUFF */
  def tryToCauseConfusion(p: Pokemon) {
    if (confusionMap contains p) {
      // Do nothing - a confused Pokemon can't become re-confused
    } else {
      confusionMap(p) = Utils.intBetween(BattleStatusManager.minTurnsConfusion, BattleStatusManager.maxTurnsConfusion + 1)
    } 
  }

  def tryToSeed(p: Pokemon) {
    if (seededSet contains p) {
      // Do nothing - a seeded Pokemon can't become re-seeded
    } else {
      seededSet += p 
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


  def processSwitchOut(p : Pokemon) = {
    // TODO: take care of everything that needs to be removed, zeroed, etc. when Pokemon p switches out of battle
  }
  
  def processTurnEnd() = {
    // TODO: Do everything that happens when the turn ends
    flinchSet.clear()  // Flinches last for exactly 1 turn
  }
}

object BattleStatusManager {
  val minTurnsConfusion = 1
  val maxTurnsConfusion = 4
}