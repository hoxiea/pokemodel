package pokemodel

import scala.collection.mutable

class MoveHistory {
  private val moveResults: mutable.Stack[MoveResult] = mutable.Stack[MoveResult]()

  def mostRecent: MoveResult = moveResults.top
  def addToHistory(mr: MoveResult) { moveResults.push(mr) }
  def clearMostRecent { moveResults.pop }
}
