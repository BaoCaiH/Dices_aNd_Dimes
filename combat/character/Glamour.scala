package combat.character

import combat.hexaGrid.{HexaGrid, HexaGridPos}
import combat.race._

class Glamour(
               name: String,
               race: Race,
               initialStat: Vector[Int],
               initialPosition: HexaGridPos,
               board: HexaGrid,
               initialHitPoint: Int = 0,
               initialExperiencePoint: Int = 85000
             )
  extends Bard(
    name, race, initialStat, initialPosition, board, initialHitPoint, initialExperiencePoint
  ) {
  override val classBranch: String = "Glamour"

  /** Return help message tailored for classes and races. */
  override def helpMessage: String = {
    val common = super.helpMessage
    common + "\n" +
      "Glamour's specific commands\n" +
      "\taction 2 [target's name]: draw a rapier and attack a target as classy as possible\n"
  }

  override protected def meleeOrRange(mOR: String): Int = math.max(this.stat("cha"), super.meleeOrRange(mOR))

  //  /** If attack roll is successful, pierce the target with a rapier sword (method inflictDmg).
  //   * If not successful, the target is not affected.
  //   *
  //   * @param target a character. */
  private def rapier(target: Character): String = {
    val (atkDice, atkRoll): (Int, Int) = this.attackRoll("melee")
    if (this.isSucceed(target, atkRoll)) {
      this.inflictDmg(target, this.diceSet.roll(this.diceSet.d6) + this.meleeOrRange("melee") + this.isCriticalHit(atkDice, this.diceSet.d6) * 2, "piercing")
      s"With a rapier in ${this.name}'s hands, he stabbed at ${target.name}'s torso. That must did some damages!\n"
    } else s"${this.name} swung a rapier, but he missed the target!\n"
  }

  protected def callAction(target: Character, n: Int): (Boolean, String) = {
    n match {
      case 0 =>
        this.drinkPotion()
      case 1 =>
        if (this.distance(target) > 60) {
          (false, "The target seems to be too far away to do this, try something else or move closer.\n")
        } else {
          (true, this.dissonantWhisper(target))
        }
      case 2 =>
        if (this.distance(target) > 5) {
          (false, "The target seems to be too far away to do this, try something else or move closer.\n")
        } else {
          (true, this.rapier(target))
        }
      case _ => (false, "Unknown action, try something else.\n")
    }
  }
}
