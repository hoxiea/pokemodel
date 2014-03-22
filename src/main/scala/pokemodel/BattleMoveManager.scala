package pokemodel

import scala.collection.mutable

/*
 * A BattleMoveManager is responsible for storing the last Move that each
 * active Pokemon has used.  Actually, the move's index is stored (in 1..165).
 * 
 * TODO: switch should remove that Pokemon's entry in lastMoveIndex when the Pokemon is switched out
 * TODO: using a move m should update lastMoveIndex with m.index for that Pokemon 
 *  getLastMove 
 * 
 * This was implemented because Mirror Move needs it, but maybe it'll be useful elsewhere.
 */

class BattleMoveManager (team1: PokemonTeam, team2: PokemonTeam) {
  private val allPokemon = team1.team ++ team2.team
  private val lastMoveIndex: mutable.Map[Pokemon, Int] = mutable.Map()
  
  def getLastMove(p: Pokemon) : Option[Move] = {
    if (lastMoveIndex.contains(p)) None // TODO: take lastMoveIndex(p), look up that move, and return a Some(new thatMove)
    else None
  }
  
  def clearLastMove(p: Pokemon) : Unit = {
    if (lastMoveIndex.contains(p)) lastMoveIndex -= p
  }
}