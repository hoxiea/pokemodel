package pokemodel

import org.scalatest.FunSuite
import Type._
import TestingInfrastructure._

class DamageCalculatorSuite extends FunSuite {

  def fixture =
    new {
      // Common Pokemon, only one type
      val rattata = new Pokemon(new PokemonBuilder("Rattata", 50))
      val machamp = new Pokemon(new PokemonBuilder("Machamp", 50))
      val pikachu = new Pokemon(new PokemonBuilder("Pikachu", 50))
      val alakazam = new Pokemon(new PokemonBuilder("Alakazam", 50))

      // Multiple types, especially unique combinations
      val venusaur = new Pokemon(new PokemonBuilder("Venusaur", 50)
                                   .maxOut()
                                   .move(1, new TestPhysicalSingleStrike))
      val golbat = new Pokemon(new PokemonBuilder("Golbat", 50))  // Poison/Flying
      val haunter = new Pokemon(new PokemonBuilder("Haunter", 50))  // Ghost/Poison
      val aero = new Pokemon(new PokemonBuilder("Aerodactyl", 50))  // Rock/Flying
      val paras = new Pokemon(new PokemonBuilder("Paras", 50))  // Bug/Grass
      val dragonite = new Pokemon(new PokemonBuilder("Dragonite", 50))  // Dragon/Flying
      val charizard = new Pokemon(new PokemonBuilder("Charizard", 50))  // Fire/Flying

      // Needed for some damageCalc methods
      val team1 = new PokemonTeam(pikachu)
      val team2 = new PokemonTeam(venusaur)
      val trainer1 = new UseFirstAvailableMove(team1)
      val trainer2 = new UseFirstAvailableMove(team2)
      val battle = new Battle(trainer1, trainer2)
    }

  test("STAB Bonus for Pokemon with only one type") {
    val f = fixture
    import f._
    assert(battle.dc.stabBonus(pikachu, new TestPhysicalSingleStrike) == 1.0)
    assert(battle.dc.stabBonus(pikachu, new TestPhysicalSingleStrike with PsychicT) == 1.0)
    assert(battle.dc.stabBonus(pikachu, new TestPhysicalSingleStrike with Ice) == 1.0)
    assert(battle.dc.stabBonus(pikachu, new TestPhysicalSingleStrike with Electric) == 1.5)
  }

  test("STAB Bonus for Pokemon with two types") {
    val f = fixture
    import f._
    assert(battle.dc.stabBonus(venusaur, new TestPhysicalSingleStrike) == 1.0)
    assert(battle.dc.stabBonus(venusaur, new TestPhysicalSingleStrike with PsychicT) == 1.0)
    assert(battle.dc.stabBonus(venusaur, new TestPhysicalSingleStrike with Ice) == 1.0)
    assert(battle.dc.stabBonus(venusaur, new TestPhysicalSingleStrike with Electric) == 1.0)
    assert(battle.dc.stabBonus(venusaur, new TestPhysicalSingleStrike with Grass) == 1.5)
    assert(battle.dc.stabBonus(venusaur, new TestPhysicalSingleStrike with Poison) == 1.5)
  }

  test("Type effectiveness, defender is Normal/Normal") {
    val f = fixture
    import f._
    assert(battle.dc.calculateTypeMultiplier(Normal, rattata) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Fighting, rattata) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Ground, rattata) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Ghost, rattata) == 0.0)
    assert(battle.dc.calculateTypeMultiplier(Electric, rattata) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Ice, rattata) == 1.0)
  }

  test("Type effectiveness, defender is Fighting/Fighting") {
    val f = fixture
    import f._
    assert(battle.dc.calculateTypeMultiplier(Normal, machamp) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Fighting, machamp) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Flying, machamp) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Ground, machamp) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Ghost, machamp) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Psychic, machamp) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Electric, machamp) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Ice, machamp) == 1.0)
  }

  test("Type effectiveness, defender is Grass/Poison") {
    val f = fixture
    import f._
    assert(battle.dc.calculateTypeMultiplier(Normal, venusaur) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Fighting, venusaur) == 0.5)
    assert(battle.dc.calculateTypeMultiplier(Flying, venusaur) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Ground, venusaur) == 1.0)  // cancel each other out
    assert(battle.dc.calculateTypeMultiplier(Ghost, venusaur) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Psychic, venusaur) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Electric, venusaur) == 0.5)
    assert(battle.dc.calculateTypeMultiplier(Ice, venusaur) == 2.0)
  }

  test("Type effectiveness, defender is Poison/Flying") {
    val f = fixture
    import f._
    assert(battle.dc.calculateTypeMultiplier(Normal, golbat) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Fighting, golbat) == 0.25)  // double
    assert(battle.dc.calculateTypeMultiplier(Flying, golbat) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Ground, golbat) == 0.0)
    assert(battle.dc.calculateTypeMultiplier(Ghost, golbat) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Psychic, golbat) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Electric, golbat) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Ice, golbat) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Grass, golbat) == 0.25)  // double
  }

  test("Type effectiveness, defender is Ghost/Poison") {
    val f = fixture
    import f._
    assert(battle.dc.calculateTypeMultiplier(Normal, haunter) == 0.0)
    assert(battle.dc.calculateTypeMultiplier(Fighting, haunter) == 0.0)
    assert(battle.dc.calculateTypeMultiplier(Flying, haunter) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Ground, haunter) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Ghost, haunter) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Psychic, haunter) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Electric, haunter) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Ice, haunter) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Grass, haunter) == 0.5)
  }

  test("Type effectiveness, defender is Rock/Flying") {
    val f = fixture
    import f._
    assert(battle.dc.calculateTypeMultiplier(Normal, aero) == 0.5)
    assert(battle.dc.calculateTypeMultiplier(Fighting, aero) == 1.0) // cancel
    assert(battle.dc.calculateTypeMultiplier(Flying, aero) == 0.5)
    assert(battle.dc.calculateTypeMultiplier(Ground, aero) == 0.0)
    assert(battle.dc.calculateTypeMultiplier(Ghost, aero) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Psychic, aero) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Electric, aero) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Ice, aero) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Grass, aero) == 1.0)  // double
  }

  test("Type effectiveness, defender is Bug/Grass") {
    val f = fixture
    import f._
    assert(battle.dc.calculateTypeMultiplier(Normal, paras) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Fighting, paras) == 0.5)
    assert(battle.dc.calculateTypeMultiplier(Flying, paras) == 4.0)  // BOOM
    assert(battle.dc.calculateTypeMultiplier(Poison, paras) == 4.0)  // BOOM
    assert(battle.dc.calculateTypeMultiplier(Fire, paras) == 4.0)  // BOOM
    assert(battle.dc.calculateTypeMultiplier(Ground, paras) == 0.25) // BOOM
    assert(battle.dc.calculateTypeMultiplier(Ghost, paras) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Psychic, paras) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Electric, paras) == 0.5)
    assert(battle.dc.calculateTypeMultiplier(Ice, paras) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Grass, paras) == 0.25)  // double
  }

  test("Type effectiveness, defender is Dragon/Flying") {
    val f = fixture
    import f._
    assert(battle.dc.calculateTypeMultiplier(Normal, dragonite) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Fighting, dragonite) == 0.5)
    assert(battle.dc.calculateTypeMultiplier(Flying, dragonite) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Poison, dragonite) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Fire, dragonite) == 0.5)
    assert(battle.dc.calculateTypeMultiplier(Ground, dragonite) == 0.0)
    assert(battle.dc.calculateTypeMultiplier(Ghost, dragonite) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Psychic, dragonite) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Electric, dragonite) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Ice, dragonite) == 4.0)  // BOOM
    assert(battle.dc.calculateTypeMultiplier(Grass, dragonite) == 0.25)  // double
  }

  test("Type effectiveness, defender is Fire/Flying") {
    val f = fixture
    import f._
    assert(battle.dc.calculateTypeMultiplier(Normal, charizard) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Fighting, charizard) == 0.5)
    assert(battle.dc.calculateTypeMultiplier(Flying, charizard) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Poison, charizard) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Fire, charizard) == 0.5)
    assert(battle.dc.calculateTypeMultiplier(Ground, charizard) == 0.0)
    assert(battle.dc.calculateTypeMultiplier(Ghost, charizard) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Psychic, charizard) == 1.0)
    assert(battle.dc.calculateTypeMultiplier(Electric, charizard) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Ice, charizard) == 2.0)
    assert(battle.dc.calculateTypeMultiplier(Grass, charizard) == 0.25)  // double
  }

  test("Plain old damage formula with numbers I found online") {
    // This example appeared at http://www.serebii.net/games/damage.shtml
    // But they did the math wrong, so I emailed the webmaster
    // With the correct math, the answer is around 120
    // And http://www.math.miami.edu/~jam/azure/compendium/battdam.htm confirms that
    // the max damage that can be done by this move is 120, which would correspond to the
    // fact that the serebii example assumes the RNG spit out a 100
    val f = fixture
    import f._
    assert(battle.dc.damageFormula(100, 219, 289, 60, 1.5, 2.0, 1.0) == 120)
  }

  test("More math.miami examples of damage calculations given damage calc inputs") {
    /* I decided to just start punching in random values and using a max-RNG
     * each time to compare my answers to his
     */
    val f = fixture
    import f._
    assert(battle.dc.damageFormula(100, 300, 289, 60, 1.5, 2, 1.0) == 162)
    assert(battle.dc.damageFormula(100, 500, 289, 60, 1.5, 2, 1.0) == 266)
    assert(battle.dc.damageFormula(100, 200, 289, 70, 1.5, 2, 1.0) == 126)
    assert(battle.dc.damageFormula(100, 200, 289, 70, 1.0, 2, 1.0) == 84)
    assert(battle.dc.damageFormula(100, 200, 153, 70, 1.0, 4, 1.0) == 312)
    assert(battle.dc.damageFormula(75, 400, 350, 60, 1.5, 0.5, 1.0) == 33)
    assert(battle.dc.damageFormula(80, 400, 270, 60, 1.5, 1.0, 1.0) == 93)
  }

  test("Venusaur values") {
    /* I decided to just start punching in random values and using a max-RNG
     * each time to compare my answers to his
     */
    val f = fixture
    import f._
    assert(battle.dc.damageFormula(50, 133, 101, 40, 1.0, 1, 0.85) == 21)
    assert(battle.dc.damageFormula(50, 133, 101, 40, 1.0, 1, 1.0) == 25)
  }

  test("AlwaysCritHit should always result in a CritHit") {
    val f = singleMoveFixture(new TestPhysicalSingleStrike with AlwaysCritHit)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.critHit)
  }

  test("NeverCritHit should never result in a CritHit") {
    val f = singleMoveFixture(new TestPhysicalSingleStrike with NeverCritHit)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(!result.critHit)
  }
}
