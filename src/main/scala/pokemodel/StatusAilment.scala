package pokemodel

object StatusAilment extends Enumeration {
  type Type = Value
  val SLP = Value("SLP")
  val PSN = Value("PSN")
  val BRN = Value("BRN")
  val FRZ = Value("FRZ")
  val PAR = Value("PAR")

  
  val stringToStatusAilment: Map[String, StatusAilment.Value] = Map(
    "SLP" -> SLP,
    "PSN" -> PSN,
    "BRN" -> BRN,
    "FRZ" -> FRZ,
    "PAR" -> PAR
  )
}
