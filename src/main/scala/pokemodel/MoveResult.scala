package pokemodel

/*
 * After implementing Moves that mutated Battles/Pokemon but didn't return any
 * values, it became apparent that knowing how much damage was done, whether
 * you hit or not, KO, etc. could be useful. This class just captures all that
 * good stuff.
 *
 * These are actually built up in the following way:
 * - DamageCalculator determines whether or not a critHit happened, and how
 * much damage is dealt (Physical and Special only)
 * - Since it integrates STAB and typeMult into the formula, it has those.
 * - It could figure out if there was a KO, but it doesn't. That's done instead in...
 * -
 */

class MoveResult (val damageDealt: Int,  // how much damage was dealt?
                  val critHit: Boolean,  // did you get a critical hit?
                  val STAB: Boolean,     // was there a STAB in play?
                  val typeMult: Double,  // what was your move's type effectiveness against defender?
                  val statusChange : Option[StatusAilment],  // did you cause a status change? if so, which one?
                  val KO: Boolean,       // did you knock the active Pokemon on the other team out?
                  val selfKO: Boolean) { // did you knock yourself out using this move?

  override def toString: String = {
    val repr = new StringBuilder()
    repr.append(s"damageDealt = $damageDealt\n")
    repr.append(s"critHit = $critHit\n")
    repr.append(s"STAB = $STAB\n")
    repr.append(s"typeMult = $typeMult\n")
    repr.append(s"statusChange = $statusChange\n")
    repr.append(s"KO = $KO\n")
    repr.append(s"selfKO = $selfKO\n")
    repr.toString()
  }
}

class MoveResultBuilder {
  // default values
  var damageDealt = 0
  var critHit = false
  var STAB = false
  var typeMult = 1.0
  var statusChange: Option[StatusAilment] = None
  var KO = false
  var selfKO = false

  val validTypeMults: Set[Double] = Set(0.0, 0.25, 0.5, 1.0, 2.0, 4.0)

  def damageDealt(x: Int): MoveResultBuilder = { damageDealt = x ; this}
  def critHit(c: Boolean): MoveResultBuilder = { critHit = c ; this}
  def STAB(s: Boolean): MoveResultBuilder = { STAB = s ; this}

  def typeMult(t: Double): MoveResultBuilder = {
    require(validTypeMults contains t, "MoveResultBuilder.typeMult was given an invalid value")
    typeMult = t
    this
  }

  def statusChange(sa: StatusAilment): MoveResultBuilder = { statusChange = Some(sa) ; this}
  def resetStatusChange: MoveResultBuilder = { statusChange = None ; this}
  def KO(k: Boolean): MoveResultBuilder = { KO = k ; this }
  def selfKO(k: Boolean): MoveResultBuilder = { selfKO = k ; this }

  /* This is a crucial method here.
   * Chained traits are passed a MoveResultBuilder as a parameter, and they
   * also typically get another MRB from DamageCalculator.calc. We'd like to
   * combine the contents of the two MRBs by merging the other's informatio
   * into the caller.
   *
   * I originally wrote this to create a new MRB and return it, immutable style,
   * but since you only get a result if the move hits, I was left without a way
   * to handle both cases in just one call. For example,
   * trait SingleStrike extends Move {
   *   abstract override def moveSpecificStuff(..., mrb: MoveResultBuilder) = {
   *     if (moveHits) {
   *       val result = pb.dc.calc(attacker, defender, this, pb)
   *       defender.takeDamage(result.damageDealt)
   *       val combined = mrb.merge(result)
   *       super.moveSpecificStuff(attacker, defender, pb, combined)
   *     } else {
   *       super.moveSpecificStuff(attacker, defender, pb, mrb)
   *     }
   *   }
   * }
   *
   * I guess two calls isn't so bad, but I saw that after I changed merge to
   * mutate the caller, so we'll keep it like that for now. Mutation is a big
   * part of the MRB experience, for better or worse
   */
  def merge(other: MoveResultBuilder) {
    damageDealt(damageDealt max other.damageDealt)
    critHit(critHit || other.critHit)
    STAB(STAB || other.STAB)
    KO(KO || other.KO)
    selfKO(selfKO || other.selfKO)

    // The typeMult is interesting. The default value is 1.0, and
    // DamageCalculator spits out the correct value. No move should
    // ever deal multiple kinds of damage simultaneously, so we
    // should check to see if $other has a more interesting value (not 1.0)
    // than we do and update if so
    val newTypeMult =
      if (other.typeMult != 1.0) other.typeMult
      else typeMult  // other is 1.0, so we're at least as interesting
    typeMult(newTypeMult)

    // No move in the game should be constructed by stacking multiple
    // status-changes, so this probably won't ever be an issue, but he's a
    // reasonable way to handle it
    val newStatusChangeOption =
      if (statusChange.isDefined) statusChange
      else if (other.statusChange.isDefined) other.statusChange
      else None
    if (newStatusChangeOption.isDefined)
      statusChange(newStatusChangeOption.get)
  }

  def toMoveResult: MoveResult = {
    new MoveResult(damageDealt, critHit, STAB, typeMult, statusChange, KO, selfKO)
  }

  override def toString: String = {
    val repr = new StringBuilder()
    repr.append(s"damageDealt = $damageDealt\n")
    repr.append(s"critHit = $critHit\n")
    repr.append(s"STAB = $STAB\n")
    repr.append(s"typeMult = $typeMult\n")
    repr.append(s"statusChange = $statusChange\n")
    repr.append(s"KO = $KO\n")
    repr.append(s"selfKO = $selfKO\n")
    repr.toString()
  }
}
