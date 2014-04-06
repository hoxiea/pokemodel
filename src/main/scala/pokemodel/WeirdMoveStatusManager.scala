package pokemodel

import scala.collection.mutable

/*
 * A WeirdMoveStatusManager handles all of the strange moves that require data structures
 * for various things.
 * 
 * Note: These are only statuses of individual moves. Something like CONFUSED, which
 * many moves can cause, is handled in 
 // TODO: finish the above comment
 */

class WeirdMoveStatusManager (team1: PokemonTeam, team2: PokemonTeam) {
  private val allPokemon = team1.team ++ team2.team

  /*
   * MIST
   * This StatusMove protects the user from stat modifications inflicted by the
   * opponent until the user switches out.
   *
   * It doesn't do anything to existing mods
   * It doesn't prevent the stat mods from BRN and PAR
   * It doesn't prevent the user from lowering its own stats
   * It fails if you use it and already have it cast
   * It can be removed by Haze and by switching out.
   *
   * The logic that actually puts Mist to work is in
   * StatManager.canChangeDefenderStats: the attacker can change the defender's
   * stats iff the defender doesn't have Mist cast. And EnemyStatChange checks for
   * this before trying anything.
   */
  private val mistSet = mutable.Set[Pokemon]()
  def hasMist(p : Pokemon) : Boolean = mistSet contains p

  def tryToRegisterMist(p: Pokemon): Boolean = {
    if (mistSet contains p) {
      // no stacking involved, fails if you already have it cast
      false
    } else {
      mistSet += p
      true
    }
  }

  def tryToRemoveMist(p: Pokemon): Boolean = {
    // Useful for Haze
    if (mistSet contains p) {
      mistSet -= p
      true
    } else false
  }

  /*
   * REFLECT
   */
  private val reflectSet = mutable.Set[Pokemon]()
  def hasReflect(p : Pokemon) : Boolean = reflectSet contains p

  def tryToRegisterReflect(p: Pokemon): Boolean = {
    if (hasReflect(p)) {
      // no stacking involved, fails if you already have it cast
      false
    } else {
      reflectSet += p
      true
    }
  }

  def tryToRemoveReflect(p: Pokemon): Boolean = {
    // Useful for Haze and switch out
    if (hasReflect(p)) {
      reflectSet -= p
      true
    } else false
  }

  /*
   * LIGHTSCREEN
   * 
   */
  private val lightScreenSet = mutable.Set[Pokemon]()
  def hasLightScreen(p : Pokemon) : Boolean = lightScreenSet contains p

  def tryToRegisterLightScreen(p: Pokemon): Boolean = {
    if (hasLightScreen(p)) {
      // no stacking involved, fails if you already have it cast
      false
    } else {
      lightScreenSet += p
      true
    }
  }

  def tryToRemoveLightScreen(p: Pokemon): Boolean = {
    // Useful for Haze and switch out
    if (hasLightScreen(p)) {
      lightScreenSet -= p
      true
    } else false
  }

  /*
   * CONVERSION
   * This is a move that only Porygon knows.
   * It changes his Types to be those of his opponent.
   */
  private val conversionSet  = mutable.Set[Pokemon]()
  def usedConversion(p: Pokemon) = conversionSet contains p

  def registerConversion(p: Pokemon): Boolean = {
    if (p.index != 137)   // Porygon
      throw new Exception("someone other than Porygon using Conversion")
    if (!conversionSet.contains(p)) conversionSet += p
    true
  }

  def tryToDeregisterConversion(p: Pokemon): Boolean = {
    if (p.index != 137)   // Porygon
      throw new Exception("someone other than Porygon dereg Conversion")
    if (conversionSet.contains(p)) {
      conversionSet -= p
      true
    } else false
  }

  /*
   * FOCUSENERGY
   * This move is supposed to increase your critical hit rate by a factor of 4.
   * An unfortunate Gen1 bug means that it actually decreased your critical hit
   * by a factor of 4. Oops.
   *
   * The effect of Focus Energy cannot stack, and it will fail if the user is
   * already under its effect. So we just need to keep track of who has it cast.
   */
  private val focusEnergySet = mutable.Set[Pokemon]()
  def hasFocusEnergy(p : Pokemon): Boolean = focusEnergySet contains p

  def tryToRegisterFocusEnergy(p: Pokemon): Boolean = {
    if (focusEnergySet contains p) {
      // no stacking involved, fails if you already have it cast
      false
    } else {
      focusEnergySet += p
      true
    }
  }

  def tryToRemoveFocusEnergy(p: Pokemon): Boolean = {
    if (focusEnergySet contains p) {
      focusEnergySet -= p
      true
    } else false
  }

  /*
   * RAGE
   * Rage is a move that lasts until the end of the battle, locking you into the
   * raging Pokemon. Every time you get hit, 
   */

  /*
   * DISABLE
   * Disable, as the name suggests, disables one of the enemy's usable moves.
   * - Only moves with PP > 0 that aren't already disabled can be targeted.
   * - Disable will fail if the target has no PP for any of its moves.
   * - Only one move per Pokemon can be disabled at any one point in time
   * 
   * Once disable has picked a move, it actually needs to disable every instance
   * of that move that the target knows. Having multiple copies of a Move is
   * rare but possible through such moves as Mimic. So pick a Move, then
   * disable Moves with that moveIndex.
   *
   * - All instances of the Move are disabled for the same 0-6 turns, random
   * - This count is decremented every time the target attempts to execute an attack. (Move.startUsingMove?)
   */

  // Each Pokemon has a Map moveIndex -> number of turns it's disabled for
  // For example, if Pokemon p had two copies of some move to be disabled for 5 turns, 
  // in moveSlots 2 and 4, then disabledMoveMap would be (p -> (2 -> 5, 4 -> 5))
  val disabledMoveMap = mutable.Map[Pokemon, mutable.Map[Int, Int]]()  // TODO: make private after testing

  def canBeDisabled(p: Pokemon) = !disabledMoveMap.contains(p)

  def isDisabled(p: Pokemon, moveslot: Int) =
    disabledMoveMap.contains(p) && disabledMoveMap(p).contains(moveslot)

  private def addToDisabledMoveMap(p: Pokemon, moveslot: Int, numTurns: Int) {
    require(0 <= numTurns && numTurns <= 6, "illegal numTurns to disable")
    if (disabledMoveMap contains p)
      disabledMoveMap(p) += (moveslot -> numTurns)
    else
      disabledMoveMap(p) = mutable.Map(moveslot -> numTurns)
  }

  def tryToDisableAMove(p: Pokemon, battle: Battle): Boolean = {
    // Returns whether or not a random disable-able move was disabled
    if (!canBeDisabled(p)) return false

    val slotOptions = p.moveslotsCanUse(battle)
    val result = 
      if (slotOptions.isEmpty) false
      else {
        val turnsDisabled = Utils.intBetween(0, 7)
        val targetMoveslot = Utils.intBetween(0, slotOptions.length)
        val allMoveslotsToDisable = List(1, 2, 3)    // TODO: get the moveslots of every instance of Pokemon.getMove(targetMoveslot); it'll probably have length 1, but do it anyway
        for (ms <- allMoveslotsToDisable) {
          addToDisabledMoveMap(p, ms, turnsDisabled)
        }
        true
      }
    result
  }


  /*
   * Useful general methods
   */
  def processSwitchOut(p: Pokemon) {
    tryToRemoveMist(p)
    tryToRemoveLightScreen(p)
    tryToRemoveReflect(p)
    tryToRemoveFocusEnergy(p)
    tryToDeregisterConversion(p)
  }
}
