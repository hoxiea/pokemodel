package pokemodel

object StatusAilment extends Enumeration {
  type StatusAilment = Value
  val SLP = Value("SLP")    // Sleep
  val PSN = Value("PSN")    // Poison
  val BPSN = Value("PSN")   // Bad Poison
  val BRN = Value("BRN")    // Burn
  val FRZ = Value("FRZ")    // Freeze
  val PAR = Value("PAR")    // Paralyzed
}
