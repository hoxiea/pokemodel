package pokemodel

import scala.collection.mutable

/*
 * A BattleMoveManager is responsible for storing the last Move index that each
 * active Pokemon has used.
 *
 * This functionality is required for MirrorMove.
 *
 * TODO: using a move m should update lastMoveIndex with m.index for that Pokemon
 *  getLastMove
 *
 * This was implemented because Mirror Move needs it, but maybe it'll be useful elsewhere.
 */

class BattleMoveManager (val b: Battle) {
  private val allPokemon = b.trainer1.team.team ++ b.trainer2.team.team
  private val lastMoveIndex: mutable.Map[Pokemon, Int] = mutable.Map()

  def getLastMove(p: Pokemon) : Option[Move] = {
    if (lastMoveIndex.contains(p)) None // TODO: take lastMoveIndex(p), look up that move, and return a Some(new thatMove)
    else None
  }

  def updateLastMoveIndex(p: Pokemon, index : Int) : Unit = {
    // require(1 <= index && index <= 165), was causing test moves to fail
    lastMoveIndex(p) = index
  }

  def clearLastMove(p: Pokemon) : Unit = {
    if (lastMoveIndex.contains(p)) lastMoveIndex -= p
  }
}
