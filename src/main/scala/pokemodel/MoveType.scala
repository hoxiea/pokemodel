package pokemodel

object MoveType extends Enumeration {
  type MoveType = Value
  val PHYSICAL = Value("PHYSICAL")
  val SPECIAL  = Value("SPECIAL ")
  val STATUS   = Value("STATUS")
}
