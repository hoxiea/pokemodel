package pokemodel

import org.scalatest.FlatSpec

class MoveFlatSpec extends FlatSpec {

  def singleMoveFixture(m: Move) =
    new {
      val pb1 = new PokemonBuilder("Charizard", 100).maxOut().move(1, m)
      val charizard = new Pokemon(pb1)
      val pb2 = new PokemonBuilder("Venusaur", 100).maxOut().move(1, "substitute")
      val venusaur = new Pokemon(pb2)
      val team1 = new PokemonTeam(charizard)
      val team2 = new PokemonTeam(venusaur)
      val trainer1 = new UseFirstAvailableMove(team1)
      val trainer2 = new UseFirstAvailableMove(team2)
      val battle = new Battle(trainer1, trainer2)
      // don't actually attack, so that you can make mutations first
    }

  /* HELPER FUNCTIONS */
  def totalDamageDealt(msResult: MoveResult) =
    // For a MultiStrike move, damageCalc is the value returned by the
    // DamageCalculator, which would ideally be how much damage is dealt
    // numTimesHit times. But MultiStrike moves are designed to stop if they
    // break a sub or KO the opponent. This function tells you how much damage
    // the MultiStrike move whose result is msResult dealt total.
    (msResult.numTimesHit - 1) * msResult.damageCalc + msResult.damageDealt

  def reduceHPTo(p: Pokemon, newHP: Int) {
    // Quick way to reduce the HP of Pokemon p down to newHP
    p.takeDamage(p.currentHP() - newHP)
    assert(p.currentHP() == newHP)
  }


  /* MULTISTRIKE */
  "A Multistrike move" should "hit between 2-5 times" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(2 <= result.numTimesHit && result.numTimesHit <= 5)
  }

  it should "register only the damage dealt on the last blow in damageDealt" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(result.damageDealt <= result.damageCalc)
  }

  it should "deal total damage as given by totalDamageDealt on a full-health enemy" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(venusaur.maxHP - venusaur.currentHP() == totalDamageDealt(result))
  }
  
  it should "deal total damage as given by totalDamageDealt on an almost-dead enemy" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike)
    import f._
    val enemyHP = 30
    reduceHPTo(venusaur, enemyHP)
    val result = charizard.useMove(1, venusaur, battle)  // now use the move
    assert(totalDamageDealt(result) == enemyHP)
  }

  it should "use the damage from a Critical Hit for all strikes" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike with AlwaysCritHit)
    import f._
    val result = charizard.useMove(1, venusaur, battle)  // now use the move
    assert(venusaur.maxHP - venusaur.currentHP() == totalDamageDealt(result))
  }

  it should "only record one strike if it kills the opponent with the first strike" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike)
    import f._
    val enemyHP = 10
    reduceHPTo(venusaur, enemyHP)
    val result = charizard.useMove(1, venusaur, battle)  // now use the move
    assert(result.numTimesHit == 1)
  }

  it should "stop if it kills a substitute without hurting the underlying Pokemon" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike with Power120)
    import f._
    val maxHP = 363   // printed out Venusaur once to check
    assert(venusaur.maxHP == maxHP)  

    // 363/4 == 90 == cost of making a substitute
    // So get it to 100 HP, then make sub => sub has 91 HP, venusaur has 10 HP
    val enemyHP = 100
    reduceHPTo(venusaur, enemyHP)
    venusaur.makeSub()
    assert(venusaur.hasSub)
    assert(venusaur.currentHP(false) == (maxHP / 4) + 1)
    assert(venusaur.currentHP(true) == enemyHP - (maxHP / 4))

    val result = charizard.useMove(1, venusaur, battle)  // now use the move

    assert(result.numTimesHit <= 2)     // Power120 always kills in 1 or 2 strikes
    assert(!result.KO)                  // Underlying venusaur is fine
    assert(venusaur.currentHP() == 10)  // Underlying venusaur is fine
    assert(result.subKO)       // Sub is dead
    assert(!venusaur.hasSub)   // Sub is reset
  }
  

  /* DOUBLESTRIKE */
  "A DoubleStrike move" should "hit a full-HP enemy exactly 2 times" in {
    val f = singleMoveFixture(new TestSpecialDoubleStrike)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(2 == result.numTimesHit)
  }

  it should "deal total damage as given by totalDamageDealt on a full-health enemy" in {
    val f = singleMoveFixture(new TestSpecialDoubleStrike)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(venusaur.maxHP - venusaur.currentHP() == totalDamageDealt(result))
  }

  it should "hit a low-HP enemy exactly 1 times" in {
    val f = singleMoveFixture(new TestSpecialDoubleStrike)
    import f._

    val enemyHP = 10
    venusaur.takeDamage(venusaur.maxHP - enemyHP)  // get it down to 10 HP
    assert(venusaur.currentHP() == enemyHP)

    val result = charizard.useMove(1, venusaur, battle)
    assert(1 == result.numTimesHit)
  }

  it should "deal total damage as given by totalDamageDealt on a low-health enemy" in {
    val f = singleMoveFixture(new TestSpecialDoubleStrike)
    import f._

    val enemyHP = 10
    venusaur.takeDamage(venusaur.maxHP - enemyHP)  // get it down to 10 HP
    assert(venusaur.currentHP() == enemyHP)

    val result = charizard.useMove(1, venusaur, battle)
    assert(enemyHP == totalDamageDealt(result))
  }

  it should "use the damage from a Critical Hit for both strikes" in {
    val f = singleMoveFixture(new TestSpecialDoubleStrike with AlwaysCritHit)
    import f._
    val result = charizard.useMove(1, venusaur, battle)  // now use the move
    assert(venusaur.maxHP - venusaur.currentHP() == result.damageCalc * 2)
  }
  
  it should "stop after 1 strike if it breaks a substitute" in {
    val f = singleMoveFixture(new TestSpecialDoubleStrike with Power200)
    import f._
    val maxHP = 363   // printed out Venusaur once to check
    assert(venusaur.maxHP == maxHP)  

    // 363/4 == 90 == cost of making a substitute
    // So get it to 100 HP, then make sub => sub has 91 HP, venusaur has 10 HP
    val enemyHP = 100
    reduceHPTo(venusaur, enemyHP)
    venusaur.makeSub()
    assert(venusaur.hasSub)
    assert(venusaur.currentHP(false) == (maxHP / 4) + 1)
    assert(venusaur.currentHP(true) == enemyHP - (maxHP / 4))

    val result = charizard.useMove(1, venusaur, battle)  // now use the move

    assert(result.numTimesHit == 1)     // Power160 always kills in 1 strike
    assert(!result.KO)                  // Underlying venusaur is fine
    assert(result.subKO)       // Sub is dead
    assert(!venusaur.hasSub)   // Sub is reset
  }

  it should "not damage the underlying Pokemon if it breaks a substitute on Strike 1" in {
    val f = singleMoveFixture(new TestPhysicalMultiStrike with Power200)
    import f._
    val maxHP = 363   // printed out Venusaur once to check
    assert(venusaur.maxHP == maxHP)  

    // 363/4 == 90 == cost of making a substitute
    // So get it to 100 HP, then make sub => sub has 91 HP, venusaur has 10 HP
    val enemyHP = 100
    reduceHPTo(venusaur, enemyHP)
    venusaur.makeSub()
    assert(venusaur.hasSub)
    assert(venusaur.currentHP(false) == (maxHP / 4) + 1)
    assert(venusaur.currentHP(true) == enemyHP - (maxHP / 4))

    val result = charizard.useMove(1, venusaur, battle)  // now use the move

    assert(result.numTimesHit == 1)     // Power200 always kills in 1 strike
    assert(venusaur.currentHP() == 10)  // Underlying venusaur is fine
  }
}
