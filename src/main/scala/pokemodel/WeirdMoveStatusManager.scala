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

class IntTracker {
  /*
   * This tracker provides a mapping of Pokemon to Ints, and lets you
   * - check for membership
   * - add to the map
   * - get the Int in the map, given a Pokemon
   * - remove from the mapping
   */

  private val members = mutable.Map[Pokemon, Int]()

  def hasProperty(p : Pokemon) : Boolean = members contains p

  def getInt(p : Pokemon) : Option[Int] = 
    if (hasProperty(p)) Some(members(p)) else None

  def tryToRegister(p: Pokemon, i: Int): Boolean = {
    // Returns whether or not p was added successfully
    if (members contains p) {
      false
    } else {
      members(p) = i
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

class IntPairTracker {
  /*
   * This tracker provides a mapping: Pokemon -> (Int, Int), and lets you:
   * - check for membership
   * - add to the map
   * - get Int1 and Int from the map, given a Pokemon
   * - decrement Int2 (Int1 doesn't change)
   * - remove from the mapping
   *
   * It's used by the ViolentStruggle moves (Thrash and Petal Dance), where we
   * store (attackerMoveslot, turnsRemaining) for any Pokemon using one of
   * these moves.
   */

  private val members = mutable.Map[Pokemon, (Int, Int)]()

  def hasProperty(p : Pokemon) : Boolean = members contains p

  def getInt1(p : Pokemon) : Option[Int] = 
    if (hasProperty(p)) Some(members(p)._1) else None

  def getInt2(p : Pokemon) : Option[Int] = 
    if (hasProperty(p)) Some(members(p)._2) else None

  def tryToRegister(p: Pokemon, pair: (Int, Int)): Boolean = {
    // Returns whether or not p was added successfully
    if (members contains p) {
      false
    } else {
      members(p) = pair
      true
    }
  }

  def tryToDecrementInt2(p: Pokemon): Boolean = {
    if (hasProperty(p)) {
      val int1 = getInt1(p).get
      val int2 = getInt2(p).get
      members(p) = (int1, int2 - 1)
      true
    } else false
  }

  def tryToRemove(p: Pokemon): Boolean = {
    // Returns whether or not p was removed successfully
    if (members contains p) {
      members -= p
      true
    } else false
  }
}


class WeirdMoveStatusManager (b: Battle) {
  def team1 = b.trainer1.team.team
  def team2 = b.trainer2.team.team
  private val allPokemon = team1 ++ team2

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
  def tryToRegisterHyperBeamDelay(p: Pokemon): Boolean = hyperbeamTracker.tryToRegister(p)
  def tryToRemoveHyperBeamDelay(p: Pokemon): Boolean = hyperbeamTracker.tryToRemove(p)


  /*
   * DIG
   * On the turn it's selected, the user digs a hole underground.
   * He then can't be hit by anything except for Swift, Bide, and Transform.
   *
   * On the following turn, the Trainer has no control over his Pokemon.
   * The 'dug' Pokemon surfaces, deals damage, has a PP deducted, and it counts
   * as the last move used.
   *
   * In terms of implementing this, there are actually two moves: RegisterDig
   * and Dig. RegisterDig updates the data structure below. The Dig attack will
   * be responsible for dealing damage, deducting PP, registering move use,
   * etc.
   *
   * Unfortunately, we can't implement this as a YesNoTracker, since we also
   * need to store the attackerMoveslot of RegisterDig.
   */
  private val digSet = new IntTracker()
  def isDug(p : Pokemon) : Boolean = digSet.hasProperty(p)
  def getRegisteredDigMoveslot(p : Pokemon) : Option[Int] = digSet.getInt(p)
  def tryToRegisterDig(p: Pokemon, moveslot: Int): Boolean = {
    require(1 <= moveslot && moveslot <= 4)
    digSet.tryToRegister(p, moveslot)
  }
  def tryToRemoveDig(p: Pokemon): Boolean = digSet.tryToRemove(p)

  /*
   * FLY
   * Exactly the same dynamics as Dig.
   */
  private val flySet = new IntTracker()
  def isFlying(p : Pokemon) : Boolean = flySet.hasProperty(p)
  def getRegisteredFlyMoveslot(p : Pokemon) : Option[Int] = flySet.getInt(p)
  def tryToRegisterFly(p: Pokemon, moveslot: Int): Boolean = {
    require(1 <= moveslot && moveslot <= 4)
    flySet.tryToRegister(p, moveslot)
  }
  def tryToRemoveFly(p: Pokemon): Boolean = flySet.tryToRemove(p)

  /*
   * SKY ATTACK
   * Exactly the same dynamics as Dig (except can take damage while charging)
   */
  private val skyAttackSet = new IntTracker()
  def isSkyAttacking(p : Pokemon) : Boolean = skyAttackSet.hasProperty(p)
  def getRegisteredSkyAttackMoveslot(p : Pokemon) : Option[Int] = skyAttackSet.getInt(p)
  def tryToRegisterSkyAttack(p: Pokemon, moveslot: Int): Boolean = {
    require(1 <= moveslot && moveslot <= 4)
    skyAttackSet.tryToRegister(p, moveslot)
  }
  def tryToRemoveSkyAttack(p: Pokemon): Boolean = skyAttackSet.tryToRemove(p)

  /*
   * SKULL BASH
   * Exactly the same dynamics as Dig (except can take damage while charging)
   */
  private val skullBashSet = new IntTracker()
  def isSkullBashing(p : Pokemon) : Boolean = skullBashSet.hasProperty(p)
  def getRegisteredSkullBashMoveslot(p : Pokemon) : Option[Int] = skullBashSet.getInt(p)
  def tryToRegisterSkullBash(p: Pokemon, moveslot: Int): Boolean = {
    require(1 <= moveslot && moveslot <= 4)
    skullBashSet.tryToRegister(p, moveslot)
  }
  def tryToRemoveSkullBash(p: Pokemon): Boolean = skullBashSet.tryToRemove(p)

  /*
   * SOLAR BEAM
   * Exactly the same dynamics as Dig (except can take damage while charging)
   */
  private val solarBeamSet = new IntTracker()
  def isSolarBeaming(p : Pokemon) : Boolean = solarBeamSet.hasProperty(p)
  def getRegisteredSolarBeamMoveslot(p : Pokemon) : Option[Int] = solarBeamSet.getInt(p)
  def tryToRegisterSolarBeam(p: Pokemon, moveslot: Int): Boolean = {
    require(1 <= moveslot && moveslot <= 4)
    solarBeamSet.tryToRegister(p, moveslot)
  }
  def tryToRemoveSolarBeam(p: Pokemon): Boolean = solarBeamSet.tryToRemove(p)

  /*
   * RAZOR WIND
   * Exactly the same dynamics as Dig (except can take damage while charging)
   */
  private val razorWindSet = new IntTracker()
  def isRazorWinding(p : Pokemon) : Boolean = razorWindSet.hasProperty(p)
  def getRegisteredRazorWindMoveslot(p : Pokemon) : Option[Int] = razorWindSet.getInt(p)
  def tryToRegisterRazorWind(p: Pokemon, moveslot: Int): Boolean = {
    require(1 <= moveslot && moveslot <= 4)
    razorWindSet.tryToRegister(p, moveslot)
  }
  def tryToRemoveRazorWind(p: Pokemon): Boolean = razorWindSet.tryToRemove(p)


  /****** VIOLENT STRUGGLES ******/
  /* THRASH */
  val thrashSet = new IntPairTracker()
  def isThrashing(p : Pokemon) : Boolean = thrashSet.hasProperty(p)

  def getRegisteredThrashMoveslot(p : Pokemon) : Option[Int] =
    thrashSet.getInt1(p)
  def getThrashTurnsLeft(p : Pokemon) : Option[Int] =
    thrashSet.getInt2(p)

  def tryToRegisterThrash(p: Pokemon, moveslot: Int): Boolean = {
    require(1 <= moveslot && moveslot <= 4)
    require(!isThrashing(p))  // p shouldn't be selecting Register if thrashing
    val numTurns = Utils.intBetween(3, 5)  // 3 or 4, 50/50
    thrashSet.tryToRegister(p, (moveslot, numTurns))
  }

  def tryToDecrementThrashTurns(p: Pokemon): Boolean = {
    // The only way for CONFUSED to kick in is if this method decrements
    // turnsRemaining to 0; any weird status thing that cancels Thrash outright
    // will use tryToRemoveThrash. So we'll confuse here
    val success = thrashSet.tryToDecrementInt2(p)
    // TODO: CONFUSION + REMOVE IF DECREMENT TO 0
    success
  }

  def tryToRemoveThrash(p: Pokemon): Boolean =
    thrashSet.tryToRemove(p)

  /* PETAL DANCE */
  private val petalDanceSet = new IntPairTracker()
  def isPetalDancing(p : Pokemon) : Boolean = petalDanceSet.hasProperty(p)

  def getRegisteredPetalDanceMoveslot(p : Pokemon) : Option[Int] =
    petalDanceSet.getInt1(p) 
  def getPetalDanceTurnsLeft(p : Pokemon) : Option[Int] =
    petalDanceSet.getInt2(p)

  def tryToRegisterPetalDance(p: Pokemon, moveslot: Int): Boolean = {
    require(1 <= moveslot && moveslot <= 4)
    require(!isPetalDancing(p))  // p shouldn't be selecting Register if dancing
    val numTurns = Utils.intBetween(3, 5)  // 3 or 4, 50/50
    petalDanceSet.tryToRegister(p, (moveslot, numTurns))
  }

  def tryToDecrementPetalDanceTurns(p: Pokemon): Boolean =
    thrashSet.tryToDecrementInt2(p)
  def tryToRemovePetalDance(p: Pokemon): Boolean =
    petalDanceSet.tryToRemove(p)


  /******** NON-STANDARD BINARY STUFF **********/
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



  /****** MULTI-TURN STUFF *******/

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

  // Each Pokemon has a Map: moveslot -> number of turns it's disabled for
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
    // To be called by Battle when Pokemon p attemps to attack: decrement
    // the number of turns left on the disable
    if (hasMoveDisabled(p)) {
      disabledMoveMap(p).transform((moveslot, turns) => turns - 1)

      // This is the only method that decrements disable turncounts... so it's
      // here that we'll remove things that have 0 turns left
      val map = disabledMoveMap(p)
      val toRemove = map.filterKeys( moveslot => map(moveslot) == 0 )
      map --= toRemove.keys
    }
  }




  /*
   * Useful general methods
   */
  def processSwitchOut(p: Pokemon) {
    tryToRemoveMist(p)
    tryToRemoveLightscreen(p)
    tryToRemoveReflect(p)
    tryToRemoveFocusEnergy(p)
    tryToDeregisterConversion(p)
  }
}

  


