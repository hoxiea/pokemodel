package pokemodel

import org.scalatest._
import TestingInfrastructure._

class ProbabilitySuite extends FlatSpec {
  "A SingleStrike move with accuracy 0.8" should "hit about 80% of the time in 100 attempts" in {
    var numHits: Int = 0
    val numTries: Int = 100
    for (_ <- 1 to numTries) {
      val f = singleMoveFixture(new TestPhysicalSingleStrike with Accuracy80)
      import f._
      val result = charizard.useMove(1, venusaur, battle)
      if (result.numTimesHit > 0) numHits += 1
    }

    // R: sum(dbinom(65:92, 100, 0.8)) == 0.9995759
    // So 99.96% of the time, we'll be in interval [65, 92]
    assert(65 <= numHits && numHits <= 92,
      s"random test fail: hit $numHits/$numTries with accuracy 0.8")
  }

  "A MultiStrike move with accuracy 0.5" should "hit about 50% of the time in 100 attempts" in {
    var numHits: Int = 0
    val numTries: Int = 100
    for (_ <- 1 to numTries) {
      val f = singleMoveFixture(new TestPhysicalMultiStrike with Accuracy50)
      import f._
      val result = charizard.useMove(1, venusaur, battle)
      if (result.numTimesHit > 0) numHits += 1
    }

    // R: sum(dbinom(33:67, 100, 0.5)) == 0.9995912
    // So 99.96% of the time, we'll be in interval [33, 67]
    assert(33 <= numHits && numHits <= 67,
      s"random test fail: hit $numHits/$numTries with accuracy 0.5")
  }

  "A DoubleStrike move with accuracy 0.3" should "hit about 30% of the time in 100 attempts" in {
    var numHits: Int = 0
    val numTries: Int = 100
    for (_ <- 1 to numTries) {
      val f = singleMoveFixture(new TestSpecialDoubleStrike with Accuracy30)
      import f._
      val result = charizard.useMove(1, venusaur, battle)
      if (result.numTimesHit > 0) numHits += 1
    }

    // R: sum(dbinom(16:47, 100, 0.5)) == 0.9994776
    // So 99.96% of the time, we'll be in interval [16, 47]
    assert(16 <= numHits && numHits <= 47,
      s"random test fail: hit $numHits/$numTries with accuracy 0.3")
  }

  "Fissure (OneHitKO) from a faster Pokemon" should "hit about 30% of the time in 100 attempts" in {
    var numHits: Int = 0
    val numTries: Int = 100
    val m = MoveDepot("fissure")
    for (_ <- 1 to numTries) {
      val f = singleMoveFixture(m)  // Charizard can actually learn Fissure in Gen 1
      import f._
      val result = charizard.useMove(1, venusaur, battle)
      if (result.numTimesHit > 0) numHits += 1
    }

    // R: sum(dbinom(16:47, 100, 0.5)) == 0.9994776
    // So 99.96% of the time, we'll be in interval [16, 47]
    assert(16 <= numHits && numHits <= 47,
      s"random test fail: hit $numHits/$numTries with accuracy 0.3")
  }

  "Ember" should "cause BRN about 10% of the time in 100 attempts" in {
    var numHits: Int = 0
    val numTries: Int = 100
    val m = MoveDepot("ember")
    for (_ <- 1 to numTries) {
      val f = singleMoveFixture(m)
      import f._
      val result = charizard.useMove(1, venusaur, battle)
      if (venusaur.isBurned) numHits += 1
    }

    // R: sum(dbinom(1:20, 100, 0.5)) == 0.9991659
    assert(1 <= numHits && numHits <= 20,
      s"random test fail: hit $numHits/$numTries with accuracy 0.1")
  }
  
}
