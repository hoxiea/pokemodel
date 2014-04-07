package pokemodel 

import org.scalacheck._
import Prop.{forAll, BooleanOperators}
import TestingInfrastructure._

object DamageEqualsUserLevelSpec extends Properties("DamageEqualsUserLevel") {

  property("appropriate damage dealt against full-health enemy") = 
    forAll(pokemonLevel) { 
      level => {
        // p2Level = 100 means that opponent's maxHP > 100
        val f = fullFixture(level, 100, List(new TestDEUL), List(MoveDepot("substitute")))
        import f._

        val result = p1.useMove(1, p2, battle)
        result.damageCalc == level && result.damageDealt == level
      }
  }

  property("correct damageCalc and damageDealt against weakened enemy if appropriate") =
    forAll(pokemonLevel) {
      level => {
        val f = fullFixture(level, 100, List(new TestDEUL), List(MoveDepot("substitute")))
        import f._
        val enemyHP = 50
        reduceHPTo(p2, enemyHP)

        val result = p1.useMove(1, p2, battle)
        result.damageCalc == level && 
        result.damageDealt == (level min enemyHP)
      }
    }
}
