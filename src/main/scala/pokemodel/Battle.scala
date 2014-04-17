package pokemodel

import Type._

import scala.collection.mutable
import scala.util.Random

// TODO: If OneHitKO succeeds, print "One hit KO!"

class Battle(val trainer1 : Trainer, val trainer2: Trainer) {
  def team1 = trainer1.team
  def team2 = trainer2.team

  // Create and register various managers and calculators for this battle
  val statManager   = new BattleStatManager(this)
  val moveManager   = new BattleMoveManager(this)
  val counterMan    = new CounterManager(this)
  val lastMoveMan   = new LastMoveManager(this)
  val statusManager = new BattleStatusManager(this)
  val weirdMoveStatusManager = new WeirdMoveStatusManager(this)
  val dc = new DamageCalculator()

  var time: Int = 0

  /* Central method for determining if a Move hits or not */
  private def chanceHit(attacker: Pokemon, defender: Pokemon, m: Move) = {
    val attackerAccuracy = statManager.getEffectiveAccuracy(attacker)
    val defenderEvasion = statManager.getEffectiveEvasion(defender)
    m.accuracy * attackerAccuracy / defenderEvasion
  }

  def moveHits(attacker: Pokemon, defender: Pokemon, m: Move): Boolean = {
    // A single method that takes all stats, statuses, weird moves,
    // the move accuracy, and a random number in account
    Random.nextDouble < chanceHit(attacker, defender, m) &&
    !weirdMoveStatusManager.isDug(defender) &&
    !weirdMoveStatusManager.isFlying(defender)
  }

  // Determining what kind of control a Trainer has over his active Pokemon
  def outOfControl(t: Trainer): Boolean = {
    require(t == trainer1 || t == trainer2)
    weirdMoveStatusManager.isRaging(t.team.activePokemon) ||
    weirdMoveStatusManager.isThrashing(t.team.activePokemon) ||
    weirdMoveStatusManager.isPetalDancing(t.team.activePokemon) ||
    weirdMoveStatusManager.isDug(t.team.activePokemon) ||
    weirdMoveStatusManager.isFlying(t.team.activePokemon) ||
    weirdMoveStatusManager.isSolarBeaming(t.team.activePokemon) ||
    weirdMoveStatusManager.isRazorWinding(t.team.activePokemon) ||
    weirdMoveStatusManager.isSkullBashing(t.team.activePokemon) ||
    weirdMoveStatusManager.isSkyAttacking(t.team.activePokemon) ||
    weirdMoveStatusManager.hasHyperBeamDelay(t.team.activePokemon)
  }

  def semiControl(t: Trainer): Boolean = {
    require(t == trainer1 || t == trainer2)
    false
  }

  /* NON-VOLATIVE STATUS EFFECTS
   * Burn, Freeze, Paralysis, Poison, Badly Poison, and Sleep
   * These remain until the Pokemon is healed at a Pokecenter (which can't
   * happen in this simulation), or after a certain number of turns in battle
   * (Sleep). Only one at a time can affect a Pokemon
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

  if (Battle.healBefore) {
    trainer1.healAll()
    trainer2.healAll()
  }

  /* METHODS FOR MAKING THE BATTLE HAPPEN */
  def takeNextTurn() {

    var team1Fainted = false
    var team2Fainted = false

  /*
   * There are three states that you as a Trainer can face at the beginning of
   * a turn:
   * 1. No choice at all. (Rage)
   * 2. Limited choice. You can switch, or you can do things like continue a
   *    Move in progress, try to attack if your Asleep, etc.
   * 3. Full control. Use any of your available moves, or switch.
   *
   * There are therefore 9 combinations that the 2 Trainers can be in:
   * (1, 1): nothing to do, faster Pokemon goes first
   * (1, 2): switch goes first, then faster/alert Pokemon
   * (1, 3): switch goes first, then faster/alert Pokemon
   * (2, 1): switch goes first, then faster/alert Pokemon (same as (1,2))
   * (2, 2):
   * (2, 3):
   * (3, 1):
   * (3, 2):
   * (3, 3): Check priorities, then break ties with Pokemon speed
   */
  val trainer1Control =
    if (outOfControl(trainer1)) 1
    else if (semiControl(trainer1)) 2
    else 3

    // Process any status ailments that take effect at the beginning of the round: SLP, PAR

    // Check volativeStatuses and weirdStatuses to see if player1 gets to select a BattleAction
    // Process the status if not

    // Check volativeStatuses and weirdStatuses to see if player2 gets to select a BattleAction
    // Process the status if not

    // TODO: Battles are responsible for printing move stuff, so that testing isn't clogged up with move-use info
    // TODO: Draining from Leech Seed is done after continuing partial trapping damage and recurrent poison or burn damage.

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
         * Both Pokemon chose to use a Move. Higher-priority moves go first,
         * though there aren't many moves with varying priorities in Gen1.
         * Speed is used next to determine who goes first.
         * TODO: flesh out all the battle details
         */
          team1.activePokemon.useMove(i, team2.activePokemon, this)
          if (team2.activePokemon.isAlive) {
            team2.activePokemon.useMove(j, team1.activePokemon, this)
          }

      }
    }

    // TODO: make sure that the Pokemon who moved first is still alive
    // If not, then the trainer needs to send someone else out and a new turn
    // begins

    // TODO: Pokemon who attacks second needs to make sure that the move it
    // picked wasn't disabled by the Pokemon who attacked first. If it was
    // disabled, then it just fails.

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
  val focusEnergyGlitch: Boolean = true

  /*
   * In Gen 1, Recover and Softboiled fail if (user's maximum HP - user's
   * current HP) is one less than a multiple of 256.  Which is stupid. This was
   * fixed in later generations. You can fix it here if you'd like.
   */
  val recoverGlitch: Boolean = true

  /*
   * Using Explosion or Selfdestruct to break a substitute doesn't actually
   * cause the user to faint. This was fixed in Stadium.
   */
  val suicideGlitchOn: Boolean = true

  /*
   * In Gen1, casting Haze doesn't actually cure the status ailment of the caster,
   * even though it DOES cure the status ailment of the enemy.
   */
  val hazeNoStatusAilmentCureGlitch = true

  /*
   * In Gen 2 and afterwards, HyperBeam always required a recharge turn.
   * However, in Gen 1, the recharge is skipped in certain instances, as
   * detailed in ActualMoves.scala#HyperBeam.
   */
  val hyperbeamRechargeGlitch = true

  // TODO: Even >1.0 chanceOfHitting moves only hit with probability 255/256
}
