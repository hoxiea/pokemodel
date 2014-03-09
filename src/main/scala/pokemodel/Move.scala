package pokemodel

import scala.collection.mutable

// TODO: Add the fact that only centain Pokemon can learn certain moves
// Can be scraped from pages such as 
// http://bulbapedia.bulbagarden.net/wiki/Charmander_(Pok%C3%A9mon)/Generation_I_learnset

abstract class Move { 
  val accuracy : Double       // in [0.0, 1.0]
  val critHitRate : Double    // in [0.0, 1.0]
  val type1 : Type
  // val action : Action
  val power : Int
  
  val maxPP : Int
  var currentPP : Int
  
  def use(pb : Battle) = {
    // TODO: do things like
  }
  
  def restorePP(amount : Int) = {
    currentPP = maxPP.min(currentPP + amount)
  }
  
  def restorePP() = {
    currentPP = maxPP
  } 
}

//class Pound extends Move {
//  val accuracy = 1.0
//  val critHitRate = 1.0
//  val type1 = Normal
//  val maxPP = 35
//  val power = 40
//}