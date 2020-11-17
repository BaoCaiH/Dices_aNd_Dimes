package combat.character

import combat.hexaGrid.{HexaGrid, HexaGridPos}
import combat.race._

abstract class Fighter(
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

  val classBranch: String = "Fighter"
  val armorClass: Int = 19
  val actions: Int = 1
  val baseStatsProficiency: Vector[String] = Vector[String]("str", "con")
  val abilityCheckProficiency: Vector[String] = Vector[String]("acrobatic", "athletics")
  protected var winds: Int = 1
  protected var surge: Int = 1

  def secondWind(): Boolean = {
    if (this.winds > 0) {
      val additionalHp = this.diceSet.roll(this.diceSet.d10) + this.level
      this.temporaryHp += math.max(0, additionalHp - this.maxHp + this.currentHp)
      this.currentHp += math.min(additionalHp, this.maxHp - this.currentHp)
      println(s"A stream of power flown through ${this.name} like a second wind, remaining HP is: ${this.remainingHp}")
      this.winds -= 1
      true
    } else {
      println("Oh no! Looks like this is the limit...")
      false
    }
  }

  def actionSurge(): Boolean = {
    if (this.surge > 0 && this.remainingActions == 0) {
      this.replenishAction()
      println(s"Adrenaline level surged in ${this.name}'s veins, ${this.name} can do this all day, remaining action is: ${this.remainingActions}")
      this.surge -= 1
      true
    } else {
      println("Nothing happened in a while... what a let down...")
      false
    }
  }
}
