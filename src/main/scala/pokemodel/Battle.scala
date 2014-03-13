package pokemodel

object Battle {
  // Customize the battle
  
  /*
   * In Gen1, teams weren't actually healed before link battles started, and some
   * players took advantage of this fact by poisoning some or all of their Pokemon.
   * Though counter-intuitive, actually protects them from all the other non-volatile
   * status effects, since Pokemon can only have one non-volatile status effect at any 
   * point in time, and newer ones don't displace older ones.
   */
  val healBefore : Boolean = false
}

class Battle(val team1 : PokemonTeam, 
			 val player1 : Player, 
             val team2 : PokemonTeam, 
             val player2 : Player) {
  
  var time : Int = 0
  

  /* Various moves cause a stat to change up/down by one level
   * This will keep track of those levels for each Pokemon, and the values
   * will be used when doing things like calculating damage
   * http://www.serebii.net/games/stats.shtml
   */
  val statMods = Map()
  
  /* NON-VOLATIVE STATUS EFFECTS
   * Burn, Freeze, Paralysis, Poison, Badly Poison, and Sleep
   * These remain until the Pokemon is healed at a Pokecenter (which can't happen in this simulation),
   * or after a certain number of turns in battle (Sleep)
   * The Pokemon data structure stores this type of status effect in "statusAilment"
   * Only one at a time can affect a Pokemon
   */ 
  
  /* VOLATIVE STATUS EFFECTS
   * These wear off when a Pokemon is switched out, or after a certain number of turns
   * Multiple of these can affect a Pokemon simultaneously
   * Confusion (wears off after 1-4 attacking turns)
   * Flinch (one-turn, can only flinch if opponent attacks first)
   * Partially trapped (caused by Wrap, Clamp, and Fire Spin, lasts 2-5 turns)
   * Seeded (leech seed, damage transfered from target to opponent's active Pokemon)
   */
  val volativeStatuses = Map()  
  
  /* THIRD KIND
   * Sky Attack -> glowing
   * Hyper Beam -> recharging
   * Solar Beam -> taking in sunlight 
   * Substitute -> substituted
   * Skull Bash -> withdrawing
   * Razor Wind -> whipping up a whirlwind 
   */
  val weirdStatuses = Map()
  
  if (Battle.healBefore) {
    team1.healAll()
    team2.healAll()
  }
  
  def nextTurn() : Unit = {
    // Process any status ailments that take effect at the beginning of the round
       
    // Check to see if player1 gets to select a BattleAction; process whatever if not

    // Check to see if player2 gets to select a BattleAction; process whatever if not

    // Get submitted Actions from both players
    val team1Action = player1.getDecision(team1, team2)
    val team2Action = player2.getDecision(team1, team2)

    // Switches get the highest priority, so process those
    (team1Action, team2Action) match {
      case (SwitchPokemon(i), SwitchPokemon(j)) => {
        team1.switch(i)
        team1.switch(j)
      }
      case (SwitchPokemon(i), UseMove(j)) => {
        team1.switch(i)
        team2.activePokemon.useMove(j, team1.activePokemon, this)
      }
      case (UseMove(i), SwitchPokemon(j)) => {
        team1.activePokemon.useMove(i, team2.activePokemon, this)
        team2.switch(j)
      }
      case (UseMove(i), UseMove(j)) => {
        /*
         * Both Pokemon chose to use a Move. Higher-priority moves go first, though there aren't many moves
         * with varying priorities in Gen1. Speed is used next to determine who goes first.
         */
        val team1Move : Move = team1.activePokemon.getMove(i)
      }
    }
    // Process any status ailments that take effect at the end of the round
    
    // Another turn passes
    time = time + 1
  }
 
  def runBattle() : Unit = {
    while (team1.hasSomeoneAlive && team2.hasSomeoneAlive) {
      nextTurn()
    }
  }
}
