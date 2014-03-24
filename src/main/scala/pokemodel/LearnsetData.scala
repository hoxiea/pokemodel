package pokemodel

import java.io.File
import scala.io.Source
import scala.collection.mutable
import scala.util.Random

/*
 * Protip: If you pass String.split a double-quoted string, then it treats that as a regex
 * And if your splitting character happens to be |, then "|" is actually the regex for
 * "nothing or nothing", which matches everything. That's why we use single-quotes for the
 * pipe split below: single-quotes capture a character literal
 */

object LearnsetData {
  var learnsets : Map[Int, mutable.Set[Int]] = Map()
  val learnsetsPath = "/Users/hha/Dropbox/pokemodel/game_data/learnsets.csv"
  val learnsetsFile = new File(learnsetsPath)

  println("Starting the loop")
  for (line <- Source.fromFile(learnsetsFile).getLines) {
    val Array(pokemonIndex : String, moveList : String) = line.split(": ")
    val moveIndices = scala.collection.mutable.Set[Int]()
    val moves : Array[String] = moveList.split(", ")
    for (move : String <- moves) {
      val Array(name : String, moveIndex : String) = move.split('|')
      moveIndices += moveIndex.toInt
    }
    learnsets += (pokemonIndex.toInt -> moveIndices)
    println(s"Done loading moves for Pokemon $pokemonIndex")
  }

  def getLearnset(index : Int) : mutable.Set[Int] = {
    require(1 <= index && index <= 151)
    learnsets(index)
  }

  def getFourRandomMoveIndices(index : Int) : List[Int] = {
    require(1 <= index && index <= 151)
    val potentials = getLearnset(index).toList
    Random.shuffle(potentials).take(4)  // TODO: could be more efficient, this is O(#moves for Pokemon $index)
  }
}