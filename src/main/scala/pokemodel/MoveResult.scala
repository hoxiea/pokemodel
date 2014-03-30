package pokemodel

/*
 * After implementing Moves that mutated Battles/Pokemon but didn't return any values, it became apparent that
 * knowing how much damage was done, whether you hit or not, KO, etc. could be useful. This class just captures
 * all that good stuff.
 * 
 * These are actually built up in the following way:
 * - DamageCalculator determines whether or not a critHit happened, and how much damage is dealt (Physical and Special only)
 *   Since it integrates STAB and typeMult into the formula, it has those. 
 *   It could figure out if there was a KO, but it doesn't. That's done instead in...
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

  val validTypeMults: Set[Double] = Set(0.25, 0.5, 1.0, 2.0, 4.0)

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
