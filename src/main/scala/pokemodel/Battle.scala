package pokemodel

import StatusAilment._
import Type._

object Battle {
  // Battle customizations
  
  /*
   * In Gen1, teams weren't actually healed before link battles started, and some
   * players took advantage of this fact by poisoning some or all of their Pokemon.
   * Though counter-intuitive, it actually protects them from all the other non-volatile
   * status effects, since Pokemon can only have one non-volatile status effect at any 
   * point in time, and newer ones don't displace older ones. This was fixed in later
   * Generations.
   */
  val healBefore : Boolean = false
}

class Battle(val trainer1 : Trainer, val trainer2: Trainer) {
  val team1 = trainer1.team
  val team2 = trainer2.team
  
  val battleStats = new BattleStatManager(team1, team2)
  
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
    trainer1.healAll()
    trainer2.healAll()
  }
  
  def takeNextTurn() : Unit = {
    var team1Fainted = false
    var team2Fainted = false
    
    // Process any status ailments that take effect at the beginning of the round: SLP, PAR
       
    // Check volativeStatuses and weirdStatuses to see if player1 gets to select a BattleAction
    // Process the status if not

    // Check volativeStatuses and weirdStatuses to see if player2 gets to select a BattleAction
    // Process the status if not

    // Get submitted Actions from both players
    val team1Action = trainer1.getDecision(this)
    val team2Action = trainer2.getDecision(this)

    // Switches get the highest priority, so process those first in all cases where they appear
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
        team2.switch(j)
        team1.activePokemon.useMove(i, team2.activePokemon, this)
      }
      case (UseMove(i), UseMove(j)) => {
        /*
         * Both Pokemon chose to use a Move. Higher-priority moves go first, though there aren't many moves
         * with varying priorities in Gen1. Speed is used next to determine who goes first.
         * TODO: flesh out all the battle details
         */
          team1.activePokemon.useMove(i, team2.activePokemon, this)
          if (team2.activePokemon.isAlive) {
            team2.activePokemon.useMove(j, team1.activePokemon, this)
          }
        
      }
    }
    
    // Process any status ailments that take effect at the end of the round, assuming the opponent
    // didn't faint, in which case status ailments don't kick in
    if (!team2Fainted) { team1.activePokemon.takeStatusAilmentDamage() }
    if (!team1Fainted) { team2.activePokemon.takeStatusAilmentDamage() }
    
    // Another turn passes
    time = time + 1
    println(this)
  }
 
  def runBattle() : Unit = {
    while (!battleIsOver) {
      takeNextTurn()
    }
  }
  
  def battleIsOver: Boolean = { !(team1.hasSomeoneAlive && team2.hasSomeoneAlive) }
  
  override def toString() : String = {
    val s = new StringBuilder()
    s.append("----------------------\n")
    s.append(team1.activePokemon)
    s.append("\n\n")
    s.append(team2.activePokemon)
    s.append("\n")
    s.append("----------------------")
    s.toString()
  }
}
