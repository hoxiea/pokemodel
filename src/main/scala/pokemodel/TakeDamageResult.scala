package pokemodel

object TakeDamageResult extends Enumeration {
  type TakeDamageResult = Value
  val ALIVE = Value("ALIVE")
  val KO    = Value("KO")
  val SUBKO = Value("SUBKO")
}
