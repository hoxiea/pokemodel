package pokemodel

object CritHitType extends Enumeration {
  type CritHitType = Value
  val NEVER = Value("NEVER")   // for testing, 0% chance of Crit Hit
  val LOW = Value("LOW")       // the standard, 512 denominator
  val HIGH = Value("HIGH")     // the rare 64 denominator
  val ALWAYS = Value("ALWAYS") // for testing, 100% chance of Crit Hit
}
