package combat.character

import combat.hexaGrid.{HexaGrid, HexaGridPos}
import combat.race._

class ArcaneArcher(
                    name: String,
                    race: Race,
                    initialStat: Vector[Int],
                    initialPosition: HexaGridPos,
                    board: HexaGrid,
                    initialHitPoint: Int = 0,
                    initialExperiencePoint: Int = 85000
                  )
  extends Fighter(
    name, race, initialStat, initialPosition, board, initialHitPoint, initialExperiencePoint
  ) {

  override val classBranch: String = "Arcane Archer"
  override val abilityCheckProficiency: Vector[String] = Vector[String]("acrobatic", "athletics", "arcane")
  private var homingShot: Int = 2

  private def arcaneShot(target: Character): String = {
    val shots = this.level match {
      case l if l < 5 => 1
      case l if l < 11 => 2
      case l if l < 20 => 3
      case _ => 4
    }
    var actionString = ""
    for (_ <- 0 until shots) {
      val (atkDice, atkRoll) = this.attackRoll("range")
      if (this.isSucceed(target, atkRoll)) {
        this.inflictDmg(target, this.diceSet.roll(this.diceSet.d8) + this.meleeOrRange("range") + this.isCriticalHit(atkDice, this.diceSet.d8), "divine")
        actionString += s"An arrow glowed brightly from ${this.name}'s direction, it sprung toward ${target.name} as soon as it's released. The arrow hit, a brilliant light struct ${target.name} from the sky!\n"
      } else if (this.homingShot > 0) {
        this.inflictDmg(target, this.diceSet.roll(this.diceSet.d8) + this.meleeOrRange("range") + this.isCriticalHit(atkDice, this.diceSet.d8), "magic")
        this.homingShot -= 1
        actionString += s"A glowing arrow sprung toward ${target.name} and missed ${target.name} by a hair... But wait, it turned back, as if having a mind of its own and struct ${target.name} on the back, but the glow is no longer there.\n"
      } else actionString += s"With enormous effort, ${this.name}'s arrow glowed with brilliant light, shot toward ${target.name}, and missed...\n"
    }
    actionString
  }

  protected def callAction(target: Character, n: Int): (Boolean, String) = {
    n match {
      case 0 =>
        this.drinkPotion()
      case 1 =>
        if (this.distance(target) > 120) {
          (false, "The target seems to be too far away to do this, try something else or move closer.\n")
        } else {
          (true, this.arcaneShot(target))
        }
      case _ => (false, "Unknown action, try something else.\n")
    }
  }
}
