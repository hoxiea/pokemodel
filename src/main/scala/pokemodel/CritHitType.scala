package pokemodel

object CritHitType extends Enumeration {
  type CritHitType = Value
  val LOW = Value("LOW")
  val HIGH = Value("HIGH")

  val stringToStatusAilment: Map[String, CritHitType.Value] = Map(
    "LOW" -> LOW,
    "HIGH" -> HIGH
  )
}
