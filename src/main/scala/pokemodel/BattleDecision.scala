package pokemodel

// In a battle, your options are either to pick a move or switch to a different Pokemon

// That assumes that you're not in the middle of a multi-turn move or in an out-of-control situation, 
// but if you're in a situation like that, then you shouldn't even have the choice of making a 
// battle decision in the first place

// When a Pokemon has no PP left for any move, the player can still choose to Attack
// The game says "POKEMON has no moves left," then uses Struggle.
// This case will correspond to UseMove(5), which the Battle manager will handle

// I considered adding a third case class UseStruggle or something, but then the pattern
// match in the Battle manager gets much messier for something that just doesn't happen that often

sealed abstract class BattleDecision

case class UseMove(val index : Int) extends BattleDecision {
  require(1 <= index && index <= 5, 
      s"invalid index $index passed to UseMove constructor")
}

case class SwitchPokemon(val index : Int) extends BattleDecision {
  require(1 <= index && index <= 6, 
      s"invalid index $index passed to UseMove constructor")  
}