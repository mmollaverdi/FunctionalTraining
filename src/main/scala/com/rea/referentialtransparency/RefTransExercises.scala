package com.rea.referentialtransparency

object RefTransExercises {

  // How would you push out the side-effects?  How would you test the logic
  // without performing the side-effect?
  def printWinner(p1: Int, p2: Int) =
    if (p1 > p2)
      println("player one is the winner!")
    else
      println("player two is the winner")

  def winner(p1: Int, p2: Int): String = if (p1 > p2) "player one" else "player two"

  def printWinner2(p1: Int, p2: Int) = println(s"${winner(p1, p2)} is the winner!")
}
