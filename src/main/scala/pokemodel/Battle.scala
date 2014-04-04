package pokemodel

import Type._
import scala.collection.mutable

// TODO: If OneHitKO succeeds, print "One hit KO!"

class Battle(val trainer1 : Trainer, val trainer2: Trainer) {
  val team1 = trainer1.team
  val team2 = trainer2.team

  // Create and register various managers and calculators for this battle
  val statManager = new BattleStatManager(team1, team2)
  val moveManager = new BattleMoveManager(team1, team2)
  val statusManager = new BattleStatusManager(team1, team2)
  val moveHistory = new MoveHistory()
  val dc = new DamageCalculator()

  var time: Int = 0

  /* NON-VOLATIVE STATUS EFFECTS
   * Burn, Freeze, Paralysis, Poison, Badly Poison, and Sleep
   * These remain until the Pokemon is healed at a Pokecenter (which can't happen in this simulation),
   * or after a certain number of turns in battle (Sleep)
   * Only one at a time can affect a Pokemon
   * 
   * The Pokemon data structure stores this type of status effect in "statusAilment".
   * And yet we need to keep track of how long a Pokemon has been asleep for.
   * When the battle starts, register sleeping Pokemon with some number of turns to rest
   */
  val sleep = mutable.Map[Pokemon, Int]()
  for (p <- team1.team ++ team2.team
       if p.statusAilment == Some(SLP)) {
    sleep(p) = Utils.intBetween(1, 8)
  }

  /* VOLATIVE STATUS EFFECTS
   * These wear off when a Pokemon is switched out, or after a certain number of turns
   * Multiple of these can affect a Pokemon simultaneously
   * Confusion (wears off after 1-4 attacking turns)
   * Flinch (one-turn, can only flinch if opponent attacks first)
   * Partially trapped (caused by Wrap, Clamp, and Fire Spin, lasts 2-5 turns)
   * Seeded (leech seed, damage transfered from target to opponent's active Pokemon)
   * Mist (user is protected from all of opponent's stat mod changes; wears off when Pokemon switched out)
   */
  val flinch = mutable.Set[Pokemon]()

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

  /* METHODS FOR MAKING THE BATTLE HAPPEN */
  def takeNextTurn() : Unit = {
    val tp = new TurnProcessor(this)
    tp.processTurn()
    var team1Fainted = false
    var team2Fainted = false

    // Process any status ailments that take effect at the beginning of the round: SLP, PAR

    // Check volativeStatuses and weirdStatuses to see if player1 gets to select a BattleAction
    // Process the status if not

    // Check volativeStatuses and weirdStatuses to see if player2 gets to select a BattleAction
    // Process the status if not

    // TODO: Battles are responsible for printing move stuff, so that testing isn't clogged up with move-use info

    // Get submitted Actions from both players
    val team1Action = trainer1.getDecision(this)
    val team2Action = trainer2.getDecision(this)

    // Switches get the highest priority, so process those first in all cases where they appear
    (team1Action, team2Action) match {
      case (SwitchPokemon(i), SwitchPokemon(j)) => {
        team1.switch(i, this)
        team1.switch(j, this)
      }
      case (SwitchPokemon(i), UseMove(j)) => {
        team1.switch(i, this)
        team2.activePokemon.useMove(j, team1.activePokemon, this)
      }
      case (UseMove(i), SwitchPokemon(j)) => {
        team2.switch(j, this)
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

    // Process any status ailments that take effect at the end of the round,
    // assuming the opponent didn't faint, in which case status ailments don't
    // kick in
    if (!team2Fainted) { team1.activePokemon.takeStatusAilmentDamage() }
    if (!team1Fainted) { team2.activePokemon.takeStatusAilmentDamage() }

    // Another turn passes
    time = time + 1
    // if (Battle.verbose) println(this)
  }

  def runBattle() : Unit = {
    while (!battleIsOver) {
      takeNextTurn()
    }
  }

  def battleIsOver: Boolean = !(team1.hasSomeoneAlive && team2.hasSomeoneAlive)

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

object Battle {
  // Battle customizations and bug fixes

  /*
   * In Gen1, teams weren't actually healed before link battles started, and
   * some players took advantage of this fact by poisoning some or all of their
   * Pokemon.  Though counter-intuitive, it actually protects them from all the
   * other non-volatile status effects, since Pokemon can only have one
   * non-volatile status effect at any point in time, and newer ones don't
   * displace older ones. This was fixed in later Generations.
   */
  val healBefore: Boolean = false


  val verbose = true
}

object Glitch {
  // Enable/disable bugs that were in Gen 1 but are kinda lame

  /*
   * The move Focus Energy is supposed to quadruple the user's critical hit
   * rate.  In Gen 1, however, it divides it by 4 instead.
   */
  val focusEnergyHelps: Boolean = false

  /*
   * In Gen 1, Recover and Softboiled fail if (user's maximum HP - user's
   * current HP) is one less than a multiple of 256.  Which is stupid. This was
   * fixed in later generations. You can fix it here if you'd like.
   */
  val recoverBugEnabled: Boolean = true

  /*
   * Using Explosion or Selfdestruct to break a substitute doesn't actually
   * cause the user to faint. This was fixed in Stadium.
   */
  val suicideGlitchOn: Boolean = true
}
