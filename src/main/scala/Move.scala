package pokemodel

abstract class Move {
  val p : Option[Pokemon]    // a Move instance has either been assigned to one Pokemon, or not 
  val accuracy : Float       // in [0.0, 1.0]
  val critHitRate : Float    // in [0.0, 1.0]
  val type1 : Type.Value
  
  val maxPP : Int
  var currentPP : Int
  
  val action : Action
  
  def restorePP(amount : Int) = {
    currentPP = maxPP.min(currentPP + amount)
  }
  
  def restorePP() = {
    currentPP = maxPP
  } 
}

class Pound 