package pokemodel

import org.scalatest._

import Type._
import TestingInfrastructure._

class StatusChangeTraitSuite extends FlatSpec {

  /* NON VOLATILE STATUS CHANGE */
  "A NonVolatileStatusChange move" should "always cause its NVSA if chanceOfCausingAilment is 1.0" in {
    val f = singleMoveFixture(new TestAlwaysBurn)
    import f._
    val result = charizard.useMove(1, venusaur, battle)
    assert(venusaur.isBurned)
  }

  it should "not displace an existing NVSA" in {
    val f = singleMoveFixture(new TestAlwaysBurn)
    import f._

    // Paralyze Venusaur
    battle.statusManager.changeMajorStatusAilment(venusaur, new PAR())
    assert(venusaur.isParalyzed)

    // Now use a move that always causes burn
    charizard.useMove(1, venusaur, battle)
    assert(venusaur.isParalyzed)  // still paralyzed
    assert(!venusaur.isBurned)    // and not burned
  }

  // TODO: VSAs

}
