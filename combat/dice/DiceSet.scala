package combat.dice

import scala.annotation.tailrec

class DiceSet {
  private def repeat(n: Int, dice: Dice): Vector[Int] = {
    @tailrec
    def recRollN(n: Int, acc: Vector[Int]): Vector[Int] = {
      if (n == 0) acc
      else recRollN(n - 1, acc :+ dice())
    }

    val rolls = recRollN(n, Vector[Int]())
    println(s"Dice ${dice.value} rolls: " + rolls.mkString(", "))
    rolls
  }

  private def rollNDice(n: Int, dice: Dice): Int = {
    repeat(n, dice).sum
  }

  def roll(n: Int)(dice: Dice): Int = {
    rollNDice(n, dice)
  }

  def roll(dice: Dice): Int = {
    roll(1)(dice)
  }

  def rollAdvantage(dice: Dice): Int = repeat(2, dice).max

  def rollDisadvantage(dice: Dice): Int = repeat(2, dice).min

  def blessing: Int = roll(D4)

}
