package pokemodel

abstract class Player {
  def getDecision(myTeam : PokemonTeam, opposition : PokemonTeam) : BattleDecision
    // An evaluation function that considers the current status of both teams
    // and returns the selected action to take
}

class HumanPlayer extends Player {
  override def getDecision(myTeam : PokemonTeam, opposition: PokemonTeam) : BattleDecision = {
	// TODO: text interface to allow selecting a move here
    SwitchPokemon(3)
  }
}

class BrendonAI extends Player {
  private def findBestSwitch(myTeam: PokemonTeam, opposition: PokemonTeam): Option[Int] = {
    // Return Some[Int] if switching is best; if there's nobody better who's alive, return None
    Some(1)  // TODO: implement findBestSwitch
  }
  
  override def getDecision(myTeam : PokemonTeam, opposition: PokemonTeam) = {
    val myP  = myTeam.activePokemon
    val oppP = opposition.activePokemon
    
    // TODO: big hard-coded decision tree here
    UseMove(1)
  }
}