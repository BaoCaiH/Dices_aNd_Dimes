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

  /** Return help message tailored for classes and races. */
  override def helpMessage: String = {
    val common = super.helpMessage
    common + "\n" +
      "Hexblade's specific commands\n" +
      "\taction 3 [target's name]: use hexed blade to slash target\n"
  }

  override protected def meleeOrRange(mOR: String): Int = math.max(this.stat("cha"), super.meleeOrRange(mOR))

  private def hexBlade(target: Character): String = {
    val atkDice = this.attackRoll(this.hasAdvantage("melee"))
    val atkRoll = atkDice + this.meleeOrRange("melee") + this.proficiencyBonus
    println(s"Attack roll: $atkRoll")
    if (this.isSucceed(target, atkRoll)) {
      this.inflictDmg(target, this.diceSet.roll(2)(this.diceSet.d6) + this.meleeOrRange("melee") + this.isCriticalHit(atkDice, this.diceSet.d6) * 2, "slashing")
      s"An enormous blade materialized on ${this.name}'s hands, he swung it directly at ${target.name}'s torso. That must did some damages!\n"
    } else s"An enormous blade materialized on ${this.name}'s hands, but the sheer weight seems to be too much for him to handle, he missed...\n"
  }

  protected def callAction(target: Character, n: Int): (Boolean, String) = {
    n match {
      case 0 =>
        this.drinkPotion()
      case 1 =>
        if (this.distance(target) > 120) {
          (false, "The target seems to be too far away to do this, try something else or move closer.\n")
        } else {
          (true, this.eldritchBlast(target))
        }
      case 2 =>
        if (this.distance(target) > 60) {
          (false, "The target seems to be too far away to do this, try something else or move closer.\n")
        } else {
          (true, this.tollTheDead(target))
        }
      case 3 =>
        if (this.distance(target) > 5) {
          (false, "The target seems to be too far away to do this, try something else or move closer.\n")
        } else {
          (true, this.hexBlade(target))
        }
      case _ => (false, "Unknown action, try something else.\n")
    }
  }
}
