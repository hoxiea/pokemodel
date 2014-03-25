package pokemodel

object MoveType extends Enumeration {
  type MoveType = Value
  val PHYSICALMOVE = Value("PHYSICALMOVE")
  val SPECIALMOVE  = Value("SPECIALMOVE")
  val STATUSMOVE   = Value("STATUSMOVE")
}
