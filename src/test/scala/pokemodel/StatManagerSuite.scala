package pokemodel

import org.scalatest.FunSuite

class StatManagerSuite extends FunSuite {

  def fixture =
    new {
      // Create a Pokemon where stats were verified online
      val pb = new PokemonBuilder("Pikachu", 81)
                   .attackIV(8)
                   .defenseIV(13)
                   .speedIV(5)
                   .specialIV(9)
                   .attackEV(23140)
                   .defenseEV(17280)
                   .speedEV(24795)
                   .specialEV(19625)
                   .hpEV(22850)
                   .move(1, new Thunder)
      assert(pb.maxHP == 189, "(HP)")
      assert(pb.attack == 137, "(Attack)")
      assert(pb.defense == 101, "(Defense)")
      assert(pb.special == 128, "(Special)")
      assert(pb.speed == 190, "(Speed)")
      val pikachu = new Pokemon(pb)

      val venusaur = new Pokemon(new PokemonBuilder("Venusaur", 50).maxOut().move(1, new VineWhip))
      val machop = new Pokemon(new PokemonBuilder("Machop", 50).maxOut().move(1, new KarateChop))
      val team1 = new PokemonTeam(pikachu)
      val team2 = new PokemonTeam(machop)
      val trainer1 = new UseFirstAvailableMove(team1)
      val trainer2 = new UseFirstAvailableMove(team2)
      val battle = new Battle(trainer1, trainer2)

    }

  test("Stats returned with no modifications should be same as current stats") {
    val f = fixture
    import f._
    assert(battle.statManager.getEffectiveAttack(pikachu) == 137)
    assert(battle.statManager.getEffectiveDefense(pikachu) == 101)
    assert(battle.statManager.getEffectiveSpecial(pikachu) == 128)
    assert(battle.statManager.getEffectiveSpeed(pikachu) == 190)
    assert(battle.statManager.getEffectiveAccuracy(pikachu) == 1.0)
    assert(battle.statManager.getEffectiveEvasion(pikachu) == 1.0)
  }

  test("Modifying Pikachu stats should change effective stats accordingly; reset sets them back") {
    val f = fixture
    import f._
    battle.statManager.changeAttackStage(pikachu, 1)
    battle.statManager.changeDefenseStage(pikachu, 2)
    battle.statManager.changeSpecialStage(pikachu, 3)
    battle.statManager.changeSpeedStage(pikachu, -4)
    battle.statManager.changeAccuracyStage(pikachu, -5)
    battle.statManager.changeEvasionStage(pikachu, 6)

    // Looking up values manually in table at
    // http://bulbapedia.bulbagarden.net/wiki/Stats#In-battle_stats
    assert(battle.statManager.getEffectiveAttack(pikachu) == (137 * 1.5).toInt)
    assert(battle.statManager.getEffectiveDefense(pikachu) == (101 * 2.0).toInt)
    assert(battle.statManager.getEffectiveSpecial(pikachu) == (128 * 2.5).toInt)
    assert(battle.statManager.getEffectiveSpeed(pikachu) == (190 * 0.333).toInt)
    assert(battle.statManager.getEffectiveAccuracy(pikachu) == (1.0 * 3.0/8).toInt)
    assert(battle.statManager.getEffectiveEvasion(pikachu) == (1.0 * 3.0).toInt)

    battle.statManager.resetAll(pikachu)
    assert(battle.statManager.getEffectiveAttack(pikachu) == 137)
    assert(battle.statManager.getEffectiveDefense(pikachu) == 101)
    assert(battle.statManager.getEffectiveSpecial(pikachu) == 128)
    assert(battle.statManager.getEffectiveSpeed(pikachu) == 190)
    assert(battle.statManager.getEffectiveAccuracy(pikachu) == 1.0)
    assert(battle.statManager.getEffectiveEvasion(pikachu) == 1.0)

  }

  test("Changes in one test shouldn't affect other tests") {
    val f = fixture
    import f._
    assert (battle.statManager.getEffectiveAttack(pikachu) == 137)
  }

  test("Cumulative changes to Pikachu's stats") {
    val f = fixture
    import f._

    battle.statManager.changeAttackStage(pikachu, 1)
    assert(battle.statManager.getEffectiveAttack(pikachu) == (137 * 1.5).toInt)

    battle.statManager.changeAttackStage(pikachu, 2)
    assert(battle.statManager.getEffectiveAttack(pikachu) == (137 * 2.5).toInt)

    battle.statManager.changeAttackStage(pikachu, -2)
    assert(battle.statManager.getEffectiveAttack(pikachu) == (137 * 1.5).toInt)

    battle.statManager.changeAttackStage(pikachu, -2)
    assert(battle.statManager.getEffectiveAttack(pikachu) == (137 * 2.0/3).toInt)

    battle.statManager.changeAttackStage(pikachu, -3)
    assert(battle.statManager.getEffectiveAttack(pikachu) == (137 * 2.0/6).toInt)
  }

  test("Stat stages can't go above +6 or below -6") {
    val f = fixture
    import f._

    battle.statManager.changeAttackStage(pikachu, 6)
    assert(battle.statManager.getEffectiveAttack(pikachu) == (137 * 4.0).toInt)
    battle.statManager.changeAttackStage(pikachu, 1)   // push above 6
    assert(battle.statManager.getEffectiveAttack(pikachu) == (137 * 4.0).toInt) // unchanged

    battle.statManager.changeDefenseStage(pikachu, -6)
    assert(battle.statManager.getEffectiveDefense(pikachu) == (101 * 2.0/8).toInt)
    battle.statManager.changeDefenseStage(pikachu, -1)   // push below -6
    assert(battle.statManager.getEffectiveDefense(pikachu) == (101 * 2.0/8).toInt) // unchanged
  }

}
