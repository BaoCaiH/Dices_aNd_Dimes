package combat.dice

import scala.util.Random

class Dice(val value: Int) {
  override def toString: String = s"This is dice ${this.value}"

  def apply(): Int = this.roll

  private def roll: Int = {
    Random.nextInt(value) + 1
  }
}

object Dice {
  def apply(value: Int) = new Dice(value)
}
