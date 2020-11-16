package combat.character

import combat.hexaGrid.{HexaGrid, HexaGridPos}
import combat.race._

class HexBlade(
                name: String,
                race: Race,
                initialStat: Vector[Int],
                initialPosition: HexaGridPos,
                board: HexaGrid,
                initialHitPoint: Int = 0,
                initialExperiencePoint: Int = 85000
              )
  extends Warlock(
    name, race, initialStat, initialPosition, board, initialHitPoint, initialExperiencePoint
  ) {
  override val classBranch: String = "Hexblade"

  private def meleeOrRange: Int = this.stat("cha")

  private def hexBlade(target: Character): Unit = {
    val atkRoll = this.attackRoll(this.hasAdvantage("range")) + this.meleeOrRange + this.proficiencyBonus
    if (this.isSucceed(target, atkRoll)) {
      println(s"An enormous blade materialized on ${this.name}'s hands, he swung it directly at ${target.name}'s torso. That must did some damages!")
      this.inflictDmg(target, this.diceSet.roll(2)(this.diceSet.d6), "slashing")
    } else println(s"An enormous blade materialized on ${this.name}'s hands, but the sheer weight seems to be too much for him to handle, he missed...")
  }

  private def callAction(target: Character, n: Int): Boolean = {
    n match {
      case 0 =>
        if (this.distance(target) > 120) {
          println("The target seems to be too far away to do this, try something else or move closer.")
          false
        } else {
          this.eldritchBlast(target)
          true
        }
      case 1 =>
        if (this.distance(target) > 5) {
          println("The target seems to be too far away to do this, try something else or move closer.")
          false
        } else {
          this.hexBlade(target)
          true
        }
      case 2 =>
        this.drinkPotion()
    }
  }

  override def action(target: Character, n: Int): Boolean = {
    if (this.remainingActions > 0) {
      val acted = this.callAction(target, n)
      if (acted) {
        this.remainingActions -= 1
        true
      } else false
    } else {
      println("You have used up your actions in this turn!")
      false
    }
  }
}
