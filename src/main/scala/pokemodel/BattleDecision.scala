package pokemodel

// These don't account for multi-move stuff and out-of-control stuff, but if you're in a multi-move
// or Rage setting, then you shouldn't even have the choice of making a battle decision

sealed abstract class BattleDecision

case class UseMove(index : Int) extends BattleDecision {
  require(1 <= index && index <= 4, 
      s"invalid index $index passed to UseMove constructor")
}

case class SwitchPokemon(index : Int) extends BattleDecision {
  require(1 <= index && index <= 6, 
      s"invalid index $index passed to UseMove constructor")  
}