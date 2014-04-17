package pokemodel

import scala.collection.mutable

/*
 * LastMoveManager keeps track of the information needed for MirrorMove.
 *
 * Essentially, we just need a record of the last move used by the active
 * Pokemon. It starts empty, and gets cleared if the active Pokemon is
 * switched out. But as long as the active Pokemon is attacking, this
 * structure will keep track of the last Move used.
 */

class LastMoveManager(b: Battle) {
  private val allPokemon = b.trainer1.team.team ++ b.trainer2.team.team
  private val lmMap = mutable.Map[Pokemon, Int]()

  def lastMoveUsed(p: Pokemon): Option[Int] = {
    require(allPokemon contains p)
    if (lmMap contains p) Some(lmMap(p)) else None
  }

  def registerLastMoveUsed(p: Pokemon, mi: Int) = {
    // returns whether registration succeeded
    require(allPokemon contains p)
    lmMap(p) = mi
  }
  
  def processSwitchOut(p: Pokemon) {
    if (lmMap contains p) lmMap -= p
  }  
}
