package pokemodel

import scala.util.matching.Regex

abstract class Trainer(val team: PokemonTeam) {
  def healAll(): Unit = { team.healAll() }

  def getSwitch(battle: Battle) : SwitchPokemon
  // Given the current state of the Battle, which Pokemon do you want to switch to?
  // This function will be called whenever the active Pokemon is KOed
  
  def getBestAction(battle: Battle) : BattleDecision
  // Given the current state of the Battle, what do you do?

  def getDecision(battle: Battle) : BattleDecision = {
    // Combine getSwitch and getBestAction, forcing the trainer to switch when it's time to switch
    // and letting him act freely otherwise
    if (!(team.hasSomeoneAlive)) {
      throw new Exception("getDecision called on a team with no living Pokemon")
    }
    
    if (team.switchNeeded) getSwitch(battle)
    else getBestAction(battle)
  }
}

class UseFirstAvailableMove(override val team: PokemonTeam) extends Trainer(team: PokemonTeam) {
  // This AI switches to the first living Pokemon on his team if he needs to, but prefers to use
  // the first Move with PP > 0 of his active Pokemon if he can
  
  def getSwitch(battle: Battle) : SwitchPokemon = new SwitchPokemon(team.firstPokemonAliveIndex)
  
  def getBestAction(battle : Battle) : UseMove = {
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
  def getSwitch(battle: Battle) : SwitchPokemon = {
    println("Switch needed. Enter your selection, 1-6")
    var input = readInt()
    if (1 <= input && input <= 6 && team.team(input).isAlive) {
      println(s"Thanks! Switching to Pokemon $input")
      return SwitchPokemon(input)
    } else {
      println("Sorry, input invalid. Please try again.")
      getSwitch(battle)
    }
  }
  
  override def getBestAction(battle : Battle) : BattleDecision = {
    println("""|I need a BattleDecision from you!
               |Enter m{#} to use Move #, or enter s{#} to switch to Pokemon #.
               |Examples: m3 => use Move3, s5 => switch to Pokemon 5""".stripMargin)
    val moveEntry   = new Regex("""m(\d)""")
    val switchEntry = new Regex("""s(\d)""")
    val enteredLine = readLine().stripLineEnd
    enteredLine match {
      case moveEntry(i) => {
        val moveIndex = i.toInt
        if (1 <= moveIndex && moveIndex <= 4) return UseMove(moveIndex)
        else { println("Invalid move index entered; please try again."); getBestAction(battle) }
      }
      case switchEntry(i) => {
        val switchEntry = i.toInt
        if (1 <= switchEntry && switchEntry <= 6 && team.team(switchEntry).isAlive) return SwitchPokemon(switchEntry)
        else { println("Invalid switch index entered; please try again."); getBestAction(battle) }
      }
      case _ => println("Input not recognized; please try again."); getBestAction(battle) 
    }
  }
}