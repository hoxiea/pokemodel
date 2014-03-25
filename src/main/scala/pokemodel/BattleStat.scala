package pokemodel

object BattleStat extends Enumeration {
  type BattleStat = Value
  val ATTACK = Value("ATTACK")
  val DEFENSE = Value("DEFENSE")
  val SPEED = Value("SPEED")
  val SPECIAL = Value("SPECIAL")
  val ACCURACY = Value("ACCURACY")
  val EVASION = Value("EVASION")
}
