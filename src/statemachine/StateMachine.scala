package statemachine

import state.State


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Action{
 def doAction(i: Input, m: Machine): State[Machine, Unit] =
   (i,m) match {

     case(_, Machine(_,0,_)) =>
     case (Coin,Machine(false,_,_)) => m
     case (Turn, Machine(true,_,_)) => m
     case (Coin, Machine(true, candy, coin)) =>
       Machine(false, candy, coin + 1)
     case (Turn, Machine(false, candy, coin)) =>
       Machine(true, candy - 1, coin)

 }

  def doActionWithOutput = doAction()

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = inputs.map(doAction(_,Machine(true,5,0)))


}
