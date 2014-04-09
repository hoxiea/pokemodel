package pokemodel

import org.scalatest._
import TestingInfrastructure._

/*
 * This file contains tests for individual moves that have randomness
 * baked into their functionality, past just dealing a random amount of
 * damage based on the 0.85-1.0 multiplier that gets slapped on.
 *
 * For example, Metronome selects a random Move and uses it.
 */

class SpecificMoveSuite extends FlatSpec {
  
  "Metronome" should "execute a random attack" in {
    val f = fullFixture(100, 100, List(MoveDepot("metronome")), List(), "Alakazam", "Venusaur")
    import f._

    val result = p1.useMove(1, p2, battle)
    println(result)
  }
}
