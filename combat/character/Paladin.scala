package combat.character

import combat.hexaGrid.{HexaGrid, HexaGridPos}
import combat.race._

abstract class Paladin(
                        name: String,
                        race: Race,
                        initialStat: Vector[Int],
                        initialPosition: HexaGridPos,
                        board: HexaGrid,
                        initialHitPoint: Int = 0,
                        initialExperiencePoint: Int = 85000
                      )
  extends Character(
    name, race, initialStat, initialPosition, board, initialHitPoint, initialExperiencePoint
  ) {

  val classBranch: String = "Paladin"
  val armorClass: Int = 19
  val actions: Int = 1
  val baseStatsProficiency: Vector[String] = Vector[String]("wis", "cha")
  val abilityCheckProficiency: Vector[String] = Vector[String]("insight", "intimidation")
  protected var healingPool: Int = this.level * 5 //a pool of healing power that can be used for Lay On Hands until this pool is run out.
  protected var thunderHits: Int = 6 //can use Thunderous Smite spell up to 6 times.

  override def helpMessage: String = {
    val common = super.helpMessage
    common + "\n" +
      "Paladin's specific commands\n" +
      "\tlay on hand [hp] [target's name]: heal hp for target\n" +
      "\taction 1 [target's name]: smite a target with a thunderous strike\n"
  }

  /** Heal this character or another character with an input of HP, only up to the max HP level of the character.
   * Amount of HP input is deducted to the healing pool. */
  def layOnHands(target: Character, hp: Int): String = {
    val healableHp = math.min(hp, this.healingPool)
    if (this.healingPool > 0 && target == this) {
      healingPool -= healableHp
      this.heal(this, healableHp)
      s"${this.name} healed himself! HP level surged in ${this.name}'s veins.\n"
    } else if (this.healingPool > 0 && target != this) {
      healingPool -= healableHp
      this.heal(target, healableHp)
      s"${this.name} healed ${target.name}! HP level surged in ${target.name}'s veins.\n"
    } else "Meh, out of healing pool. Nothing happened...\n"
  }

  /** If attack roll is successful, deal thunder damage to the target.
   * If not successful, the target is not affected.
   * One Thunderous Smite spell needs one thunder hit. */
  protected def thunderousSmite(target: Character): String = {
    val hits = this.level match {
      case someLevel if someLevel < 5 => 1
      case _ => 2
    }
    var actionString = ""
    for (_ <- 0 until hits) {
      val (atkDice, atkRoll) = this.attackRoll("melee")
      if (this.isSucceed(target, atkRoll)) {
        this.inflictDmg(target, this.diceSet.roll(this.diceSet.d10) + this.diceSet.roll(2)(this.diceSet.d6) + this.meleeOrRange("melee") + this.isCriticalHit(atkDice, this.diceSet.d10), "thunder")
        this.thunderHits -= 1
        actionString += s"${this.name} casted Thunderous Smite on ${target.name}, ${target.name} was damaged by thunder!\n"
      } else actionString += s"${this.name} tried to inflict thunder damage on ${target.name}, but ${target.name} dodged...\n"
    }
    actionString
  }

}
