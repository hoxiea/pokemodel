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
  
  val superEffective = 2.0
  val notVeryEffective = 0.5
  val notEffective = 0.0
  
  val stringToType: Map[String, Type.Value] = Map(
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
  
  val typeMap : Map[Type.Value, Map[Type.Value, Double]] = Map(
    Normal   -> Map(Rock -> notVeryEffective, 
                    Ghost -> notEffective).withDefaultValue(1.0),
    Fighting -> Map(Normal -> superEffective, 
                    Flying -> notVeryEffective,
                      Poison -> notVeryEffective,
                      Rock -> superEffective,
                      Bug -> notVeryEffective,
                      Ghost -> notEffective,
                      Psychic -> notVeryEffective,
                      Ice -> superEffective).withDefaultValue(1.0),
    Flying -> Map(Fighting -> superEffective,
                  Rock -> notVeryEffective,
                  Bug -> superEffective,
                  Grass -> superEffective,
                  Electric -> notVeryEffective).withDefaultValue(1.0)
      ).withDefaultValue(Map().withDefaultValue(1.0))  // TODO: remove this, fill in the rest!
}
