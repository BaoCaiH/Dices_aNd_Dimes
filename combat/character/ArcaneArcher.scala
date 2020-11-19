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

  private def arcaneShot(target: Character): Unit = {
    val shots = this.level match {
      case l if l < 5 => 1
      case l if l < 11 => 2
      case l if l < 20 => 3
      case _ => 4
    }
    for (_ <- 0 until shots) {
      val atkDice = this.attackRoll(this.hasAdvantage("range"))
      val atkRoll = atkDice + this.meleeOrRange("range") + this.proficiencyBonus
      println(s"Attack roll: $atkRoll")
      if (this.isSucceed(target, atkRoll)) {
        println(s"An arrow glowed brightly from ${this.name}'s direction, it sprung toward ${target.name} as soon as it's released. The arrow hit, a brilliant light struct ${target.name} from the sky!")
        this.inflictDmg(target, this.diceSet.roll(this.diceSet.d8) + this.meleeOrRange("range") + this.isCriticalHit(atkDice, this.diceSet.d8), "divine")
      } else if (this.homingShot > 0) {
        println(s"A glowing arrow sprung toward ${target.name} and missed ${target.name} by a hair... But wait, it turned back, as if having a mind of its own and struct ${target.name} on the back, but the glow is no longer there.")
        this.inflictDmg(target, this.diceSet.roll(this.diceSet.d8) + this.meleeOrRange("range") + this.isCriticalHit(atkDice, this.diceSet.d8), "magic")
        this.homingShot -= 1
      } else println(s"With enormous effort, ${this.name}'s arrow glowed with brilliant light, shot toward ${target.name}, and missed...")
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
          this.arcaneShot(target)
          true
        }
    }
  }

}
