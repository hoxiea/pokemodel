package pokemodel

import Type._
import BattleStat._
import TakeDamageResult._

/*
 * After implementing Moves that mutated Battles/Pokemon but didn't return any
 * values, it became apparent that knowing how much damage was done, whether
 * you hit or not, KO, etc. could be useful in at least 2 cases:
 * 1. Testing. Easily making sure that the damage dealt was in the appropriate
 *    range, that a status ailment is inflicted a certain % of the time, that
 *    a substitute was KOed when it was supposed to be, etc.
 * 2. Moves that require a history.
 *
 * A MoveResult captures all the information needed for both cases.
 *
 * There are two ways to get your hands on a MoveResultBuilder, which each move
 * is responsible for returning and completing. The first is to call
 * DamageCalculator.calc, which fills in as much information as it can using
 * everything that it has.  The second is to make one from scratch, hope that
 * most of the default values apply to you, and then change the ones that
 * don't.
 *
 * The following values are provided by DamageCalculator:
 * - moveIndex
 * - rawDamage
 * - numTimesHit = 1
 * - damageDealt
 * - dUnderlying
 * - critHit
 * - STAB
 * - moveType
 * - typeMult
 *
 * That means a move/trait that uses DC should modify the following as needed:
 * - numTimesHit if it's != 1
 * - hpGained
 * - nvsa / vsa
 * - selfStat + selfStatAmount
 * - enemyStat + enemyStatAmount
 * - KO
 * - subKO
 * - selfKO
 *
 * Or, if starting from scratch, provide all relevant quantities.
 *
 * Another crucial operation on MoveResultBuilders is the ability to combine
 * them.  For example, a move like Thunder is implemented as something that's
 * both SingleStrike and StatusChange. So whatever happens with the
 * SingleStrike should get passed to the StatusChange trait, which should then
 * merge in its own information to the SingleStrike result, if necessary.
 */

class MoveResult (
  val moveIndex: Int,    // which move was used?
  val rawDamage: Int,    // what's the max damage this move could deal, from DamageCalc?
  val numTimesHit: Int,  // how many times did the move hit? usually 1
  val damageDealt: Int,  // #damage actually dealt (on the last strike)
  val dUnderlying: Int,  // #damage the last strike would had dealt to enemy, if enemy didn't have a sub
  val hpGained: Int,     // how much HP did the user gain?
  val critHit: Boolean,  // did you get a critical hit?
  val STAB: Boolean,     // was there a STAB in play?
  val moveType: Type,    // what Type was the move? (Normal, Flying, etc.)
  val typeMult: Double,  // what was your move's type effectiveness against defender?
  val nvsa: Option[NonVolatileStatusAilment], // cause status change? which?
  val vsa: Option[VolatileStatusAilment],     // cause status change? which?
  val selfStat: Option[BattleStat],     // change your own stat? which?
  val selfStatAmount: Option[Int],      // how much change your own stat?
  val enemyStat: Option[BattleStat],     // change enemy's stat? which?
  val enemyStatAmount: Option[Int],      // how much change enemy stat?
  val KO: Boolean,       // did you knock the active Pokemon on the other team out?
  val subKO: Boolean,    // did you break an opponent's substitute?
  val selfKO: Boolean) { // did you knock yourself out using this move?

  override def toString: String = {
    val repr = new StringBuilder()
    repr.append(s"moveIndex = $moveIndex (${MoveDepot(moveIndex)})\n")
    repr.append(s"rawDamage = $rawDamage\n")
    repr.append(s"numTimesHit = $numTimesHit\n")
    repr.append(s"damageDealt = $damageDealt\n")
    repr.append(s"dUnderlying = $dUnderlying\n")
    repr.append(s"hpGained = $hpGained\n")
    repr.append(s"critHit = $critHit\n")
    repr.append(s"STAB = $STAB\n")
    repr.append(s"moveType = $moveType\n")
    repr.append(s"typeMult = $typeMult\n")
    repr.append(s"nvsa = $nvsa\n")
    repr.append(s"vsa = $vsa\n")
    repr.append(s"selfStat = $selfStat\n")
    repr.append(s"selfStatAmount = $selfStatAmount\n")
    repr.append(s"enemyStat = $enemyStat\n")
    repr.append(s"enemyStatAmount = $enemyStatAmount\n")
    repr.append(s"KO = $KO\n")
    repr.append(s"subKO = $subKO\n")
    repr.append(s"selfKO = $selfKO\n")
    repr.toString()
  }
}

class MoveResultBuilder {
  // defaults: a Normal move with an invalid index and no effect on the world
  var moveIndex = -1
  var rawDamage = 0
  var numTimesHit = 0
  var damageDealt = 0
  var dUnderlying = 0
  var hpGained = 0
  var critHit = false
  var STAB = false
  var moveType = Normal
  var typeMult = 1.0
  var nvsa: Option[NonVolatileStatusAilment] = None
  var vsa: Option[VolatileStatusAilment] = None
  var selfStat: Option[BattleStat] = None
  var selfStatAmount: Option[Int] = None
  var enemyStat: Option[BattleStat] = None
  var enemyStatAmount: Option[Int] = None
  var KO = false
  var subKO = false
  var selfKO = false

  private val validTypeMults: Set[Double] = Set(0.0, 0.25, 0.5, 1.0, 2.0, 4.0)

  // Setters
  def moveIndex(x: Int): MoveResultBuilder = {
    require((1 <= x && x <= 165) || x == 999,  // 999 is the TestMove index
      "MRB.moveIndex")
    moveIndex = x
    this
  }
  def rawDamage(x: Int): MoveResultBuilder = {
    require(x >= 0, "MRB.rawDamage")
    rawDamage = x
    this
  }
  def numTimesHit(x: Int): MoveResultBuilder = {
    require(x >= 0, "MRB.numTimesHit")
    numTimesHit = x
    this
  }
  def damageDealt(x: Int): MoveResultBuilder = {
    require(x >= 0, "MRB.damageDealt")
    damageDealt = x
    this
  }
  def dUnderlying(x: Int): MoveResultBuilder = {
    require(x >= 0, "MRB.dUnderlying")
    dUnderlying = x
    this
  }
  def hpGained(x: Int): MoveResultBuilder = {
    require(x >= 0, "MRB.hpGained")
    hpGained = x
    this
  }

  def critHit(c: Boolean): MoveResultBuilder = { critHit = c ; this}
  def STAB(s: Boolean): MoveResultBuilder = { STAB = s ; this}
  def moveType(t: Type): MoveResultBuilder = { moveType = t ; this}

  def typeMult(t: Double): MoveResultBuilder = {
    require(validTypeMults contains t,
      "MoveResultBuilder.typeMult was given an invalid value")
    typeMult = t
    this
  }

  def nvsa(sa: NonVolatileStatusAilment): MoveResultBuilder = {
    nvsa = Some(sa)
    this
  }
  def vsa(sa: VolatileStatusAilment): MoveResultBuilder = {
    vsa = Some(sa)
    this
  }

  // You'll never have one without the other, so combine these two
  def addSelfStat(stat: BattleStat, amount: Int): MoveResultBuilder = {
    require (-2 <= amount && amount <= 2)   // moves only change things 1 or 2 levels in Gen 1
    selfStat = Some(stat)
    selfStatAmount = Some(amount)
    this
  }
  def addEnemyStat(stat: BattleStat, amount: Int): MoveResultBuilder = {
    require (-3 <= amount && amount <= 3)
    enemyStat = Some(stat)
    enemyStatAmount = Some(amount)
    this
  }

  def KO(k: Boolean): MoveResultBuilder = { KO = k ; this }
  def KO(p: Pokemon): MoveResultBuilder = { KO = !p.isAlive ; this }

  def subKO(k: Boolean): MoveResultBuilder = { subKO = k ; this }

  def selfKO(k: Boolean): MoveResultBuilder = { selfKO = k ; this }
  def selfKO(p: Pokemon): MoveResultBuilder = { selfKO = !p.isAlive ; this }

  // Other useful methods
  def processTakeDamageResult(defender: Pokemon, tdr: TakeDamageResult) {
      tdr match {
        case TakeDamageResult.KO => {
          KO(true)
          assert(!(defender.isAlive))
        }
        case TakeDamageResult.SUBKO => subKO(true)
        case TakeDamageResult.ALIVE => {}
      }
  }

  def checkConsistency {
    // TODO: make sure the numbers all make sense
  }


  def toMoveResult: MoveResult = {
    new MoveResult(
      moveIndex,
      rawDamage,
      numTimesHit,
      damageDealt,
      dUnderlying,
      hpGained,
      critHit,
      STAB,
      moveType,
      typeMult,
      nvsa,
      vsa,
      selfStat,
      selfStatAmount,
      enemyStat,
      enemyStatAmount,
      KO,
      subKO,
      selfKO
    )
  }

  /* merge is a crucial method here, as described in the top comment */
  def merge(other: MoveResult): MoveResultBuilder = {
    moveIndex(moveIndex     max other.moveIndex)
    rawDamage(rawDamage   max other.rawDamage)
    numTimesHit(numTimesHit max other.numTimesHit)
    damageDealt(damageDealt max other.damageDealt)
    dUnderlying(dUnderlying max other.dUnderlying)
    hpGained(hpGained       max other.hpGained)

    critHit(critHit || other.critHit)
    STAB(STAB       || other.STAB)

    val newMoveType =
      if (moveType != Normal) moveType
      else other.moveType
    moveType(newMoveType)

    // The default typeMult value is 1.0, and DamageCalculator spits out the
    // correct value. No move should ever deal multiple kinds of damage
    // simultaneously, so we should check to see if $other has a more
    // interesting value (not 1.0) than we do and update if so
    val newTypeMult =
      if (other.typeMult != 1.0) other.typeMult
      else typeMult  // other is 1.0, so we're at least as interesting
    typeMult(newTypeMult)

    // No move in the game should be constructed by stacking multiple
    // status-changes, so this probably won't ever be an issue, but he's a
    // reasonable way to handle it
    val newNvsaOption =
      if (nvsa.isDefined) nvsa  // already have one
      else if (other.nvsa.isDefined) other.nvsa // adding one
      else None
    if (newNvsaOption.isDefined) nvsa(newNvsaOption.get)

    val newVsaOption =
      if (vsa.isDefined) vsa  // already have one
      else if (other.vsa.isDefined) other.vsa // adding one
      else None
    if (newVsaOption.isDefined) vsa(newVsaOption.get)

    // I don't think we ever stack stat changes either, but let's merge them
    val newSelfStat =
      if (selfStat.isDefined) selfStat
      else if (other.selfStat.isDefined) other.selfStat
      else None
    val newSelfStatAmount =
      if (selfStatAmount.isDefined) selfStatAmount
      else if (other.selfStatAmount.isDefined) other.selfStatAmount
      else None
    if (newSelfStat.isDefined && newSelfStatAmount.isDefined)
      addSelfStat(newSelfStat.get, newSelfStatAmount.get)

    val newEnemyStat =
      if (enemyStat.isDefined) enemyStat
      else if (other.enemyStat.isDefined) other.enemyStat
      else None
    val newEnemyStatAmount =
      if (enemyStatAmount.isDefined) enemyStatAmount
      else if (other.enemyStatAmount.isDefined) other.enemyStatAmount
      else None
    if (newEnemyStat.isDefined && newEnemyStatAmount.isDefined)
      addEnemyStat(newEnemyStat.get, newEnemyStatAmount.get)

    // Finish things off
    KO(KO         || other.KO)
    subKO(subKO   || other.subKO)
    selfKO(selfKO || other.selfKO)
  }

  def merge(mrb: MoveResultBuilder): MoveResultBuilder = {
    merge(mrb.toMoveResult)
  }

  override def toString: String = {
    val repr = new StringBuilder()
    repr.append(s"moveIndex = $moveIndex (${MoveDepot(moveIndex)})\n")
    repr.append(s"rawDamage = $rawDamage\n")
    repr.append(s"numTimesHit = $numTimesHit\n")
    repr.append(s"damageDealt = $damageDealt\n")
    repr.append(s"dUnderlying = $dUnderlying\n")
    repr.append(s"hpGained = $hpGained\n")
    repr.append(s"critHit = $critHit\n")
    repr.append(s"STAB = $STAB\n")
    repr.append(s"typeMult = $typeMult\n")
    repr.append(s"nvsa = $nvsa\n")
    repr.append(s"vsa = $vsa\n")
    repr.append(s"selfStat = $selfStat\n")
    repr.append(s"selfStatAmount = $selfStatAmount\n")
    repr.append(s"enemyStat = $enemyStat\n")
    repr.append(s"enemyStatAmount = $enemyStatAmount\n")
    repr.append(s"KO = $KO\n")
    repr.append(s"subKO = $subKO\n")
    repr.append(s"selfKO = $selfKO\n")
    repr.toString()
  }
}
