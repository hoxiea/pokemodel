package pokemodel

import scala.collection.mutable
import ViolentStruggleType._

/*
 * A WeirdMoveStatusManager handles all of the strange moves that require data structures
 * for various things.
 * 
 * Note: These are only statuses of individual moves. Something like CONFUSED, which
 * many moves can cause, is handled in 
 // TODO: finish the above comment
 */

class YesNoTracker {
  /*
   * After noticing a bunch of moves that create some state that a Pokemon
   * is either in or not-in, I encapsulated the logic here.
   */
  private val members = mutable.Set[Pokemon]()
  def hasProperty(p : Pokemon) : Boolean = members contains p

  def tryToRegister(p: Pokemon): Boolean = {
    // Returns whether or not p was added successfully
    if (members contains p) {
      false
    } else {
      members += p
      true
    }
  }

  def tryToRemove(p: Pokemon): Boolean = {
    // Returns whether or not p was removed successfully
    if (members contains p) {
      members -= p
      true
    } else false
  }
}

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
   * stats iff the defender doesn't have Mist cast. And EnemyStatChange checks
   * for this before trying anything.
   */
  private val mistTracker = new YesNoTracker
  def hasMist(p : Pokemon): Boolean = mistTracker.hasProperty(p)
  def tryToRegisterMist(p: Pokemon): Boolean = mistTracker.tryToRegister(p)
  def tryToRemoveMist(p: Pokemon): Boolean = mistTracker.tryToRemove(p)

  /*
   * REFLECT
   * Cute little move that doubles the user's Defense stat (against Physical
   * attacks) until it's switched out. It's taken into account in the Stat
   * Manager's getEffectiveDefense.
   */
  private val reflectTracker = new YesNoTracker
  def hasReflect(p : Pokemon): Boolean = reflectTracker.hasProperty(p)
  def tryToRegisterReflect(p: Pokemon): Boolean = reflectTracker.tryToRegister(p)
  def tryToRemoveReflect(p: Pokemon): Boolean = reflectTracker.tryToRemove(p)

  /*
   * LIGHTSCREEN
   * Cute little move that doubles the user's Special when it's used as a
   * defensive value until it's switched out. This move inspired me to
   * implement separate getSpecialAttack() and getSpecialDefense() methods in
   * the Stat Manager; getSpecialDefense() takes LightScreen into the account,
   * so that the DamageCalculator gets the correct value.
   */
  private val lightscreenTracker = new YesNoTracker
  def hasLightscreen(p : Pokemon): Boolean = lightscreenTracker.hasProperty(p)
  def tryToRegisterLightscreen(p: Pokemon): Boolean = lightscreenTracker.tryToRegister(p)
  def tryToRemoveLightscreen(p: Pokemon): Boolean = lightscreenTracker.tryToRemove(p)

  /*
   * FOCUSENERGY
   * This move is supposed to increase your critical hit rate by a factor of 4.
   * An unfortunate Gen1 bug means that it actually decreased your critical hit
   * by a factor of 4. Oops. See Glitch.focusEnergyGlitch
   *
   * The effect of Focus Energy cannot stack, and it will fail if the user is
   * already under its effect. So we just need to keep track of who has it cast.
   */
  private val focusEnergyTracker = new YesNoTracker
  def hasFocusEnergy(p : Pokemon): Boolean = focusEnergyTracker.hasProperty(p)
  def tryToRegisterFocusEnergy(p: Pokemon): Boolean = focusEnergyTracker.tryToRegister(p)
  def tryToRemoveFocusEnergy(p: Pokemon): Boolean = focusEnergyTracker.tryToRemove(p)

  /*
   * HYPERBEAM (DELAY)
   * This is technically a 1-turn delay, so I could use a countdown tracker, except
   * the starting value would always be 1 and then it would just decrement and expire.
   * Instead, we'll treat it as a YesNo thing.
   */
  private val hyperbeamTracker = new YesNoTracker
  def hasHyperBeamDelay(p : Pokemon): Boolean = hyperbeamTracker.hasProperty(p)
  def tryToRegisterHyperBeam(p: Pokemon): Boolean = hyperbeamTracker.tryToRegister(p)
  def tryToRemoveHyperBeam(p: Pokemon): Boolean = hyperbeamTracker.tryToRemove(p)


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
  def hasMoveDisabled(p: Pokemon) = disabledMoveMap.contains(p)

  def isDisabled(p: Pokemon, moveslot: Int) =
    // used by Pokemon.canUseMove
    disabledMoveMap.contains(p) && disabledMoveMap(p).contains(moveslot)

  private def addToDisabledMoveMap(p: Pokemon, moveslot: Int, numTurns: Int) {
    require(0 <= numTurns && numTurns <= 6, "illegal numTurns to disable")
    if (disabledMoveMap contains p)
      disabledMoveMap(p) += (moveslot -> numTurns)
    else
      disabledMoveMap(p) = mutable.Map(moveslot -> numTurns)
  }

  private def allMoveslotsMatchingGivenMoveslot(p: Pokemon, ms: Int): List[Int] = {
    /* Pokemon have up to 4 moves, in moveslots 1, 2, 3, 4.
     * Rarely, a Pokemon will have the same move in multiple moveslots.
     * This function returns the list of all moveslots that contain the same
     * Move as the one at the given moveIndex
     */
    require(p.getMove(ms).isDefined, "allMoveslotsMatchingGivenMoveslot fail")
    val moveIndex = p.getMove(ms).get.index
    val result = List(1, 2, 3, 4).filter(
      i => p.getMove(i).isDefined && p.getMove(i).get.index == moveIndex
    )
    result
  }

  def tryToDisableAMove(p: Pokemon, battle: Battle): Boolean = {
    // Attempt to disable a random usable move of Pokemon $p in Battle $battle
    // Returns whether or not the disable succeeded
    if (!canBeDisabled(p)) return false

    val slotOptions = p.moveslotsCanUse(battle)
    val result = 
      if (slotOptions.isEmpty) false
      else {
        val turnsDisabled = Utils.intBetween(0, 7)

        // Pick a valid moveslot, then find all instances of the chosen move
        val targetMoveslot = Utils.intBetween(0, slotOptions.length)
        val allMoveslotsToDisable = allMoveslotsMatchingGivenMoveslot(p, targetMoveslot)

        // Add them all to disabledMoveMap
        for (ms <- allMoveslotsToDisable) {
          addToDisabledMoveMap(p, ms, turnsDisabled)
        }
        true
      }
    result
  }

  def processPokemonAttackAttempt(p: Pokemon) {
    if(hasMoveDisabled(p)) {
      // TODO: decrement everything in disabledMoveMap(p) by 1
    }
  }

  private def removeAllDisables(p: Pokemon) {
    if (disabledMoveMap.contains(p)) disabledMoveMap -= p
  }

  /*
   * VIOLENT STRUGGLE: THRASH AND PETAL DANCE
   * These two moves cause the Pokemon using them to go into a temporary
   * Rage-like state where they attack for either 3 or 4 moves.
   * While struggling, you can't switch out or do anything other than struggle.
   * Each strike does a damage calculation and can miss.
   * 
   * If the Pokemon reaches the end of its 3-4 turns, it becomes CONFUSED.
   * However, it can be interrupted by (full paralysis, hurt self due to
   * pre-existing CONFUSED), in which case you stop and no CONFUSED.
   * Sleep, freeze, partial trapping, and flinching pause but don't stop struggle.
   *
   * Since it's impossible to be using both moves simultaneously, it's enough
   * to track all Violent Struggles in one data structure, used by the trait
   * ViolentStruggle. However, the two moves have different powers and different
   * types, so we'll encode which move the Pokemon is using with a simple enum.
   */
  private val vsMap: mutable.Map[Pokemon, (ViolentStruggleType, Int)] = mutable.Map()
  def isThrashing(p: Pokemon): Boolean = false
  def isPetalDancing(p: Pokemon): Boolean = false
  def isViolentStruggling(p: Pokemon): Boolean = isThrashing(p) || isPetalDancing(p)



  /*
   * Useful general methods
   */
  def processSwitchOut(p: Pokemon) {
    tryToRemoveMist(p)
    tryToRemoveLightscreen(p)
    tryToRemoveReflect(p)
    tryToRemoveFocusEnergy(p)
    tryToDeregisterConversion(p)
    removeAllDisables(p)  // TODO: do disables actually clear when you switch out?
  }
}

  


