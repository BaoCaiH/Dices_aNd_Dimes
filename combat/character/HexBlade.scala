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
    val atkDice = this.attackRoll(this.hasAdvantage("melee"))
    val atkRoll = atkDice + this.meleeOrRange + this.proficiencyBonus
    println(s"Attack roll: $atkRoll")
    if (this.isSucceed(target, atkRoll)) {
      println(s"An enormous blade materialized on ${this.name}'s hands, he swung it directly at ${target.name}'s torso. That must did some damages!")
      this.inflictDmg(target, this.diceSet.roll(2)(this.diceSet.d6) + this.meleeOrRange + this.isCriticalHit(atkDice, this.diceSet.d6) * 2, "slashing")
    } else println(s"An enormous blade materialized on ${this.name}'s hands, but the sheer weight seems to be too much for him to handle, he missed...")
  }

  override protected def eldritchBlast(target: Character): Unit = {
    for (_ <- 0 to ((this.level - 5) / 6) + 1) {
      val atkDice = this.attackRoll(this.hasAdvantage("range"))
      val atkRoll = atkDice + this.spellAttack
      println(s"Attack roll: $atkRoll")
      if (this.isSucceed(target, atkRoll)) {
        println(s"A blast struct ${target.name} like a rhino!")
        this.inflictDmg(target, this.diceSet.roll(this.diceSet.d12) + this.meleeOrRange + this.isCriticalHit(atkDice, this.diceSet.d12), "force")
      } else println("A blast beamed ahead like a mad beast, but it missed...")
    }
  }

  protected def callAction(target: Character, n: Int): Boolean = {
    n match {
      case 0 =>
        this.drinkPotion()
      case 1 =>
        if (this.distance(target) > 120) {
          println("The target seems to be too far away to do this, try something else or move closer.")
          false
        } else {
          this.eldritchBlast(target)
          true
        }
      case 2 =>
        if (this.distance(target) > 5) {
          println("The target seems to be too far away to do this, try something else or move closer.")
          false
        } else {
          this.hexBlade(target)
          true
        }
    }
  }

  //  override def action(target: Character, n: Int): Boolean = {
  //    if (this.remainingActions > 0) {
  //      val acted = this.callAction(target, n)
  //      if (acted) {
  //        this.remainingActions -= 1
  //        true
  //      } else false
  //    } else {
  //      println("You have used up your actions in this turn!")
  //      false
  //    }
  //  }
}
