package combat.dice

import scala.annotation.tailrec

class DiceSet {
  val d2: Dice = D2
  val d4: Dice = D4
  val d6: Dice = D6
  val d8: Dice = D8
  val d10: Dice = D10
  val d12: Dice = D12
  val d20: Dice = D20
  val d100: Dice = D100

  private def repeat(n: Int, dice: Dice): Vector[Int] = {
    @tailrec
    def recRollN(n: Int, acc: Vector[Int]): Vector[Int] = {
      if (n == 0) acc
      else recRollN(n - 1, acc :+ dice())
    }

    val rolls = recRollN(n, Vector[Int]())
    println(s"\tDice ${dice.value} rolls: " + rolls.mkString(", "))
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

  def rollAdvantage(dice: Dice): Int = {
    println("Advantage, take the larger roll!")
    repeat(2, dice).max
  }

  def rollDisadvantage(dice: Dice): Int = {
    println("Disadvantage, take the smaller roll...")
    repeat(2, dice).min
  }

  def blessing: Int = roll(d4)

  def apply(dice: Dice): Int = this.roll(dice)

}
