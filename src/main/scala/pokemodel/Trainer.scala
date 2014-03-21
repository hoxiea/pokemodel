package pokemodel

abstract class Trainer(val team: PokemonTeam) {
  def healAll(): Unit = { team.healAll() }

  def getDecision(battle: Battle) : BattleDecision
    // An evaluation function that considers the current status of the battle
    // and returns the desired action to take
    // Different extensions of Trainer will implement this in different ways
}

class UseFirstAvailableMove(override val team: PokemonTeam) extends Trainer(team: PokemonTeam) {
  override def getDecision(battle : Battle) : BattleDecision = {
    team.activePokemon.move1 match {
      case Some(move) if (move.currentPP > 0) => return new UseMove(1)
      case None => team.activePokemon.move2 match {
        case Some(move) if (move.currentPP > 0) => return new UseMove(2)
        case None => team.activePokemon.move3 match {
          case Some(move) if (move.currentPP > 0) => return new UseMove(3)
          case None => team.activePokemon.move4 match {
            case Some(move) if (move.currentPP > 0) => return new UseMove(4)
            case None => return new UseMove(5)
          }
        } 
      }
    } 
  }
}

class HumanPlayer(override val team: PokemonTeam) extends Trainer(team: PokemonTeam) {
  override def getDecision(battle : Battle) : BattleDecision = {
	// TODO: text interface to allow selecting a move here
    SwitchPokemon(3)
  }
}

class BrendonAI(override val team: PokemonTeam) extends Trainer(team: PokemonTeam) {
  private def findBestSwitch(battle : Battle): Option[Int] = {
    // Return Some[Int] if switching is best; if there's nobody better who's alive, return None
    Some(1)  // TODO: implement findBestSwitch
  }

  override def getDecision(battle : Battle) = {
    UseMove(1)
  }
}
