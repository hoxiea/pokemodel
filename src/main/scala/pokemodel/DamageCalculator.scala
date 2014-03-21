package pokemodel

import scala.util.Random

object DamageCalculator {
  def criticalHit(m : Move) : Boolean = false
  
  def calc(attacker : Pokemon, moveIndex : Int, defender : Pokemon) : Int = {
    // If attacker uses the Move at moveIndex, how much damage would it do against defender?
    // from http://www.math.miami.edu/~jam/azure/compendium/battdam.htm
    // TODO: take battle statistics into account!  Attack/Defense/whatever might have increased/decreased
    // TODO: the order is Type Effectiveness, then STAB, then critical hit... but shouldn't really matter?
    require(1 <= moveIndex && moveIndex <= 4, 
        s"invalid moveIndex $moveIndex passed to DamageCalculator.calc")
    return 0
  }
  
  def calculateTypeMultiplier(attack : Move, d : Pokemon) : Double = {
    val attackType = attack.type1
    val dType1 = d.type1
    val dType2 = d.type2
    val m = Type.typeMap(attackType)
    if (dType1 == dType2) m(dType1)
    else m(dType1) * m(dType2)
  }
  
  def stabBonus(attacker : Pokemon, move : Move) : Double = {
    if (attacker.type1 == move.type1 || attacker.type2 == move.type1) 1.5 
    else 1.0
  }
}