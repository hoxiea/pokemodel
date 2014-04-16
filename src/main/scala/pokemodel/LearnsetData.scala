package pokemodel

import java.io.File
import scala.io.Source
import scala.collection.mutable
import scala.util.Random

/*
 * Protip: If you pass String.split a double-quoted string, then it treats that
 * as a regex And if your splitting character happens to be |, then "|" is
 * actually the regex for "nothing or nothing", which matches everything.
 * That's why we use single-quotes for the pipe split below: single-quotes
 * capture a character literal
 */

object LearnsetData {
  var learnsets: Map[Int, mutable.Set[Int]] = Map()
  val learnsetsPath = "/Users/hha/Dropbox/pokemodel/game_data/learnsets.csv"
  val learnsetsFile = new File(learnsetsPath)

  for (line <- Source.fromFile(learnsetsFile).getLines) {
    val Array(pokemonIndex: String, moveList: String) = line.split(": ")
    val moveIndices = mutable.Set[Int]()
    val moves: Array[String] = moveList.split(", ")
    for (move: String <- moves) {
      val Array(name: String, moveIndex: String) = move.split('|')
      moveIndices += moveIndex.toInt
    }
    learnsets += (pokemonIndex.toInt -> moveIndices)
  }

  def getLearnset(index: Int): mutable.Set[Int] = {
    require(1 <= index && index <= 151)
    learnsets(index)
  }

  def getFourRandomMoveIndices(index: Int): List[Int] = {
    require(1 <= index && index <= 151)
    val potentials = getLearnset(index).toList
    Random.shuffle(potentials).take(4)
    // FACT: take returns the whole list if fewer than 4 elements, so no worries there
    // TODO: could be more efficient, this is O(#moves for Pokemon $index)
  }

  def getPokemonIndexWhoCanLearn(mi: Int): Int = {
    // given moveIndex mi, return the index of a Pokemon that can learn Move mi
    require(1 <= mi && mi <= 165)
    for ((pi, learnset) <- learnsets) {
      if (learnset contains mi)
        return pi
    }
    return -1
  }
}
