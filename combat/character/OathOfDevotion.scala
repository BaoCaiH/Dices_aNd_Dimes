package combat.character

import combat.hexaGrid.{HexaGrid, HexaGridPos}
import combat.race._

class OathOfDevotion(
                      name: String,
                      race: Race,
                      initialStat: Vector[Int],
                      initialPosition: HexaGridPos,
                      board: HexaGrid,
                      initialHitPoint: Int = 0,
                      initialExperiencePoint: Int = 85000
                    )
  extends Paladin(
    name, race, initialStat, initialPosition, board, initialHitPoint, initialExperiencePoint
  ) {

  override val classBranch: String = "Oath of devotion"
  private var spellSlot: Int = 2

  override protected def meleeOrRange(mOR: String): Int = math.max(this.stat("cha"), super.meleeOrRange(mOR))

  /** Return help message tailored for classes and races. */
  override def helpMessage: String = {
    val common = super.helpMessage
    common + "\n" +
      "Oath of Devotion's specific commands\n" +
      "\taction 2 [target's name]: smite a target with a divine strike\n"
  }

  //  /** If attack roll is successful, deal radiant damage to the target.
  //   * If not successful, the target is not affected.
  //   * One Divine Smite spell needs one spell slot.
  //   *
  //   * @param target a character. */
  protected def divineSmite(target: Character): String = {
    val hits = this.level match {
      case someLevel if someLevel < 5 => 1
      case _ => 2
    }
    var actionString = ""
    for (_ <- 0 until hits) {
      val (atkDice, atkRoll) = this.attackRoll("melee")
      if (this.isSucceed(target, atkRoll)) {
        this.inflictDmg(target, this.diceSet.roll(this.diceSet.d10) + this.diceSet.roll(3)(this.diceSet.d8) + this.meleeOrRange("melee") + this.isCriticalHit(atkDice, this.diceSet.d10), "radiant")
        this.spellSlot -= 1
        actionString += s"${this.name} bashed ${target.name}, ${target.name} was damaged by radiant!\n"
      } else actionString += s"${this.name} tried to inflict radiant damage on ${target.name}, but ${target.name} dodged...\n"
    }
    actionString
  }

  protected def callAction(target: Character, n: Int): (Boolean, String) = {
    n match {
      case 0 =>
        this.drinkPotion()
      case 1 =>
        if (this.distance(target) > 5) {
          (false, "The target seems to be too far away to do this, try something else or move closer.\n")
        } else {
          (true, this.thunderousSmite(target))
        }
      case 2 =>
        if (this.distance(target) > 5) {
          (false, "The target seems to be too far away to do this, try something else or move closer.\n")
        } else {
          (true, this.divineSmite(target))
        }
      case _ => (false, "Unknown action, try something else.\n")
    }
  }
}
