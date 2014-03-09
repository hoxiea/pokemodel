package pokemodel

sealed abstract class Type {
  val against : Map[Type, Double]
}

case object Normal extends Type {
  val against = Map(Rock -> 0.5, Ghost -> 0.0).withDefaultValue(1.0)
}

case object Fighting extends Type {
  val against = Map(Normal -> 2.0, 
                    Flying -> 0.5,
                    Poison -> 0.5,
                    Rock -> 2.0,
                    Bug -> 0.5,
                    Ghost -> 0.0,
                    Psychic -> 0.5,
                    Ice -> 2.0).withDefaultValue(1.0)
}

case object Flying extends Type {
  val against = Map(Fighting -> 2.0,
                    Rock -> 0.5,
                    Bug -> 2.0,
                    Grass -> 2.0,
                    Electric -> 0.5).withDefaultValue(1.0)
}

case object Poison extends Type {
  val against = Map(Fighting -> 2.0,
                    Rock -> 0.5,
                    Bug -> 2.0,
                    Grass -> 2.0,
                    Electric -> 0.5).withDefaultValue(1.0)
}
case object Poison extends Type
case object Ground extends Type
case object Rock extends Type
case object Bug extends Type
case object Ghost extends Type
case object Fire extends Type
case object Water extends Type
case object Grass extends Type
case object Electric extends Type
case object Psychic extends Type
case object Ice extends Type
case object Dragon extends Type

object Type2 extends Enumeration {
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
  
  val typeMap : Map[Type.Value, Map[Type.Value, Double]] = Map(
    Normal   -> Map(Rock -> 0.5, 
                    Ghost -> 0.0).withDefaultValue(1.0),
    Fighting -> Map(Normal -> 2.0, 
                    Flying -> 0.5,
                      Poison -> 0.5,
                      Rock -> 2.0,
                      Bug -> 0.5,
                      Ghost -> 0.0,
                      Psychic -> 0.5,
                      Ice -> 2.0).withDefaultValue(1.0),
    Flying -> Map(Fighting -> 2.0,
                  Rock -> 0.5,
                  Bug -> 2.0,
                  Grass -> 2.0,
                  Electric -> 0.5).withDefaultValue(1.0)
      ).withDefaultValue(Map().withDefaultValue(1.0))  // TODO: remove this, fill in the rest!
}
