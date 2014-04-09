package pokemodel

import scala.collection.mutable
import Battle.{verbose => VERBOSE}
import Type._

/*
 * A BattleStatusManager keeps track of all of the various volatile/weird status and
 * provides useful methods for accessing this information.
 *
 * It also encapsulates the logic behind changing a Pokemon's single non-volatile status
 * ailment, even though you can just change it directly via a setter.
 */

// TODO: Everything in http://www.smogon.com/rb/articles/differences

/* TODO: From http://www.smogon.com/rb/articles/differences
 * Partial-Trapping Moves
 * Wrap, Fire Spin, Clamp, and Bind prevent your
 * opponent from attacking after they hit during the whole 2-5 turns the attack
 * may last instead of preventing switches. So, if you are faster than your
 * opponent, you can keep using Wrap over and over again and they cannot hit
 * you back. Only the first hit of the 2-5 attack sequence wastes a PP and is
 * subject to miss due to accuracy. The Wrapping Pokemon isn't allowed to use
 * any other attack until the 2-5 turn sequence ends. In addition, a critical
 * hit is only calculated for the first hit; if it does score a critical hit,
 * every remaining sequence hit will do that amount of damage. Using a trapping
 * move can also be used to get a free switch. For example: Cloyster uses Clamp
 * on an incoming Starmie. Cloyster can switch out freely the turn after while
 * Starmie will stay immobilized for that turn, but will still be released from
 * the Clamp. The Pokemon will also remain immobilized if the user of the
 * partial trapping technique is fully paralyzed mid sequence, but the sequence
 * will end.  If a Wrapped Pokemon switches out while in the middle of a Wrap
 * sequence, the sequence resets, and the accuracy must be tested again on the
 * switch-in and another PP is wasted. If at such a time the trapping move has
 * 0 PP, it will still be used against the incoming Pokemon. After that use,
 * the current PP of the trapping move will roll over to 63.  If the target of
 * the partial trapping move just used Hyper Beam, it won't have to recharge if
 * the partial trapping move misses on the recharge turn. Additionally, if the
 * user of the partial trapping move attacks before the user of Hyper Beam
 * during a recharge turn and the partial trapping move misses, the user of
 * Hyper Beam will automatically use Hyper Beam during that turn. If at such a
 * time Hyper Beam has 0 PP, Hyper Beam will still be used, and its PP will
 * roll over 63.  A final interesting note concerning only Wrap and Bind, is
 * that while as they are Normal-type attacks and deal no damage when used
 * against a Ghost-type, they will still immobilize it.
 */

class BattleStatusManager (val team1 : PokemonTeam, val team2: PokemonTeam) {
  /*
   * Structures for tracking things that last a certain (random) number of turns
   */
  private val sleepMap     = mutable.Map[Pokemon, Int]()
  private val partiallyTrappedMap = mutable.Map[Pokemon, Int]()

  /*
   * Structures for things that get worse with time, and need to track their progression
   */
  private val badlyPoisonedMap = mutable.Map[Pokemon, Int]()

  /*
   * Structures for tracking yes/no stuff
   * A Pokemon appearing in one of these means that that status ailment is in
   * effect
   */
  // Status Ailments
  private val flinchSet = mutable.Set[Pokemon]()

  // Attacking moves
  private val skyAttackSet  = mutable.Set[Pokemon]()
  private val skullBashSet  = mutable.Set[Pokemon]()
  private val flySet  = mutable.Set[Pokemon]()
  private val digSet  = mutable.Set[Pokemon]()


  /*
   * CONFUSION
   */
  private val confusionMap = mutable.Map[Pokemon, Int]()
  def hasConfusion(p: Pokemon) = confusionMap.contains(p)
  def tryToCauseConfusion(p: Pokemon): Boolean = {
    if (hasConfusion(p)) {
      if (VERBOSE) println(s"Confusion (status) had no effect of {p.name}")
      false
    } else {
      val confusionDuration =
        Utils.intBetween(BattleStatusManager.minTurnsConfusion,
                         BattleStatusManager.maxTurnsConfusion + 1)
      confusionMap(p) = confusionDuration
      if (VERBOSE)
        // TODO: don't actually print the number
        println(s"{p.name} will be confused for $confusionDuration turns")
      true
    }
  }

  def tryToRemoveConfusion(p: Pokemon): Boolean = {
    if (hasConfusion(p)) {
      confusionMap -= p
      true
    } else false
  }

  /*
   * SEEDED - LEECH SEED'S DOMAIN
   */
  private val seededSet = mutable.Set[Pokemon]()

  def tryToSeed(p: Pokemon): Boolean = {
    if (seededSet.contains(p) || p.type1 == Grass || p.type2 == Grass) {
      if (VERBOSE) println(s"Seeding had no effect on {p.name}")
      false
    } else {
      seededSet += p
      if (VERBOSE) println(s"{p.name} was seeded!")
      true
    }
  }

  def tryToRemoveSeeded(p: Pokemon): Boolean = {
    if (seededSet.contains(p)) {
      seededSet -= p
      true
    } else false
  }

  /*
   * BADLY POISONED (BPSN) - TOXIC'S DOMAIN
   */

  /*
   * PARTIALLY TRAPPED
   */
  def tryToPartiallyTrap(p: Pokemon): Boolean = {
    if (partiallyTrappedMap contains p) {
      // Do nothing - can't trap again while trapped
      false
    } else {
      partiallyTrappedMap(p) = Utils.intBetween(BattleStatusManager.minTurnsPartiallyTrapped, BattleStatusManager.maxTurnsPartiallyTrapped + 1)
      if (VERBOSE) println(s"{p.name} will be partially trapped for ${partiallyTrappedMap(p)} turns")  // TODO: don't actually print the number
      true
    }
  }

  def causeToFlinch(p: Pokemon): Boolean = {
    /*
     * Technically, only the first Pokemon to make a move during a turn can be affected by Flinch.
     * But since processTurnEnd clears flinchSet, all that happens when the second Pokemon to use a Move
     * causes a flinch is that its opponent is added to flinchSet but then is immediately cleared from it,
     * resulting in the correct fact that the opponent won't flinch on his next turn
     * So we can just use this anywhere a Flinch is needed without worrying about who attacked first
     */
    flinchSet += p
    true  // TODO: this could be better, but it's not far off
  }

  def canBeHit(p: Pokemon): Boolean = {
    !flySet.contains(p) && !digSet.contains(p)
  }

  def processSwitchOut(p : Pokemon) = {
    // TODO: take care of everything that needs to be removed, zeroed, etc. when Pokemon p switches out of battle
    tryToRemoveSeeded(p)
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
    // TODO: Go through everything that's counting turns down, and decrement them
  }

  /*
   * Non-Volatile Status Stuff
   */
  def canChangeMajorStatusAilment(p: Pokemon): Boolean = 
      p.statusAilment == None && !p.hasSub

  def changeMajorStatusAilment(p: Pokemon, newStatus: NonVolatileStatusAilment): Boolean = {
    if (!canChangeMajorStatusAilment(p))
      throw new Exception("Tried to change NVSA but you're not allowed!")

    p.statusAilment match {
      case None => { p.statusAilment = Some(newStatus); true }
      case _ => false
    }
  }
}

object BattleStatusManager {
  val minTurnsConfusion = 1
  val maxTurnsConfusion = 4
  val minTurnsPartiallyTrapped = 1
  val maxTurnsPartiallyTrapped = 4
}
