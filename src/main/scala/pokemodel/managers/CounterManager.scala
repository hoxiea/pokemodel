package pokemodel

import scala.collection.mutable

/*
 * CounterManager keeps track of the information needed for the Move Counter.
 *
 * In Gen1, a Pokemon p1 using Counter will act on the last DAMAGE DEALT to p1.
 * Switching, full paralysis, and multi-turn moves don't reset the last
 * amount of damage done.  
 * The same damage-receiving can be Countered multiple times, as long as the
 * damage-receiving isn't replaced.  
 * So we need to keep track of, for each Pokemon, the Type and DamageDealt for
 * the last DamageDealt>0 strike.
 *
 * We'll just map each Pokemon to the MoveResult that captures this info.
 */

class CounterManager(b: Battle) {
  private val allPokemon = b.trainer1.team.team ++ b.trainer2.team.team
  private val damageMap = mutable.Map[Pokemon, MoveResult]()

  def lastDamageDealtMR(p: Pokemon): Option[MoveResult] = {
    require(allPokemon contains p)
    if (damageMap contains p) Some(damageMap(p)) else None
  }

  def tryToRegisterDamageTaken(p: Pokemon, mr: MoveResult): Boolean = {
    // returns whether registration succeeded
    require(allPokemon contains p)
    if (mr.damageDealt > 0) {
      damageMap(p) = mr
      true
    } else false
  }

  def processSwitchOut(p: Pokemon) {
    // Switching out doesn't remove the last damage dealt in Gen 1
  }
}
