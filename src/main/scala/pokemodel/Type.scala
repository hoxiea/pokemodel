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

  val double = 2.0
  val normal = 1.0
  val half = 0.5
  val not = 0.0

  /*
   * Capture all of the various effectiveness of one element against another.
   * This can be read as, for example:
   * Normal against Rock is half effective
   * Normal against Ghost is not effective
   * Fighting against Rock is double(super) effective
   * ...
   */
  val typeMap : Map[Type.Value, Map[Type.Value, Double]] = Map(
    Normal   -> Map(Rock -> half,
                    Ghost -> not).withDefaultValue(normal),
    Fighting -> Map(Normal -> double,
                    Flying -> half,
                    Poison -> half,
                    Rock -> double,
                    Bug -> half,
                    Ghost -> not,
                    Psychic -> half,
                    Ice -> double).withDefaultValue(normal),
    Flying -> Map(Fighting -> double,
                  Rock -> half,
                  Bug -> double,
                  Grass -> double,
                  Electric -> half).withDefaultValue(normal),
    Poison -> Map(Poison -> half,
                  Ground -> half,
                  Rock -> half,
                  Bug -> double,
                  Ghost -> half,
                  Grass -> double).withDefaultValue(normal),
    Ground -> Map(Flying -> not,
                  Poison -> double,
                  Rock -> double,
                  Bug -> half,
                  Fire -> double,
                  Grass -> half,
                  Electric -> double).withDefaultValue(normal),
    Rock -> Map(Fighting -> half,
                Flying -> double,
                Ground -> half,
                Bug -> double,
                Fire -> double,
                Ice -> double).withDefaultValue(normal),
    Bug -> Map(Fighting -> half,
               Flying -> half,
               Poison -> double,
               Ghost -> half,
               Fire -> half,
               Grass -> double,
               Psychic -> double).withDefaultValue(normal),
    Ghost -> Map(Normal -> not,
                 Ghost -> double,
                 Psychic -> not).withDefaultValue(normal),
    Fire -> Map(Rock -> half,
                Bug -> double,
                Fire -> half,
                Water -> half,
                Grass -> double,
                Ice -> double,
                Dragon -> half).withDefaultValue(normal),
    Water -> Map(Ground -> double,
                 Rock -> double,
                 Fire -> double,
                 Water -> half,
                 Grass -> half,
                 Dragon -> half).withDefaultValue(normal),
    Grass -> Map(Flying -> half,
                 Poison -> half,
                 Ground -> double,
                 Rock -> double,
                 Bug -> half,
                 Fire -> half,
                 Water -> double,
                 Grass -> half,
                 Dragon -> half).withDefaultValue(normal),
    Electric -> Map(Flying -> double,
                    Ground -> not,
                    Water -> double,
                    Grass -> half,
                    Electric -> half,
                    Dragon -> half).withDefaultValue(normal),
    Psychic -> Map(Fighting -> double,
                   Poison -> double,
                   Psychic -> half).withDefaultValue(normal),
    Ice -> Map(Flying -> double,
               Ground -> double,
               Water -> half,
               Grass -> double,
               Ice -> half,
               Dragon -> double).withDefaultValue(normal),
    Dragon -> Map(Dragon -> double).withDefaultValue(normal)
  )
}
