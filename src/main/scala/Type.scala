package pokemodel

object Type extends Enumeration {
  type Type = Value
  val Normal = Value("Normal")
  val Fighting = Value("Fighting")
  val Flying = Value("Flying")
  val Poison = Value("Poison")
  val Ground = Value("Ground")
  val Rock = Value("Rock")
  val Bug = Value("Bug")
  val Ghost = Value("Ghost")
  val Fire = Value("Fire")
  val Water = Value("Water")
  val Grass = Value("Grass")
  val Electric = Value("Electric")
  val Psychic = Value("Psychic")
  val Ice = Value("Ice")
  val Dragon = Value("Dragon")
  
  val allTypes = List(
    Normal, Fighting, Flying, Poison, Ground, 
    Rock, Bug, Ghost, Fire, Water, 
    Grass, Electric, Psychic, Ice, Dragon
  )
  
  val stringToValue : Map[String, Type.Value] = Map(
    "Normal" -> Normal,
    "Fighting" -> Fighting,
    "Flying" -> Flying,
    "Poison" -> Poison,
    "Ground" -> Ground,
    "Rock" -> Rock,
    "Bug" -> Bug,
    "Ghost" -> Ghost,
    "Fire" -> Fire,
    "Water" -> Water,
    "Grass" -> Grass,
    "Electric" -> Electric,
    "Psychic" -> Psychic,
    "Ice" -> Ice,
    "Dragon" -> Dragon
  ) 
}

object DamageCalculator {
  def calculateDamage(attacker : Pokemon, attack : Action, defender : Pokemon) : Int = {
    return 0
  }
}