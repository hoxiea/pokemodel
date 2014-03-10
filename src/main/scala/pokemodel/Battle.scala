package pokemodel

import scala.collection.mutable.PriorityQueue

object Battle {
  // Customize the battle
  val healBefore : Boolean = false
}

class Battle(val team1 : PokemonTeam, val player1 : Player, 
             val team2 : PokemonTeam, val player2 : Player) {
  val time : Int = 0

  if (Battle.healBefore) {
    team1.healAll()
    team2.healAll()
  }
  
  
  
  // private def orderActions
  // val q = new PriorityQueue[BattleDecision]()(Ordering.by(orderActions))
  def runBattle() : Unit = {
    while (team1.hasSomeoneAlive && team2.hasSomeoneAlive) {
    // Process any status ailments that take effect at the beginning of the round
    
    // Check to see if player1 has control of his active Pokemon, process whatever if he doesn't
    
    // Check to see if player2 has control of his active Pokemon, process whatever if he doesn't
    
    // Get submitted Actions from both players
    val team1Action = player1.getDecision(team1, team2)
    val team2Action = player2.getDecision(team1, team2)
    
    // Switches get the highest priority, so process those
    
    // Process any status ailments that take effect at the end of the round
  }

  }
}