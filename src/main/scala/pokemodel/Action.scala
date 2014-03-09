package pokemodel

// TODO: Possible actions are useMove1, useMove2, useMove3, useMove4, or switch
// TODO: But these need to account for the multi-turn and out-of-control things too

sealed abstract class Action
case object Switch extends Action
case object useMove1 extends Action
case object useMove2 extends Action
case object useMove3 extends Action
case object useMove4 extends Action