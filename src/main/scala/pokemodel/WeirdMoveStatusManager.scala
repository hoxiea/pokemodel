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
   * Useful general methods
   */
  def processSwitchOut(p: Pokemon) {
    tryToRemoveMist(p)
    if (usedConversion(p)) p.resetTypes()
    tryToDeregisterConversion(p)
  }
}
