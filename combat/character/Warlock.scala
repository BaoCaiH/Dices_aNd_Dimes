package combat.character

import combat.hexaGrid.{HexaGrid, HexaGridPos}
import combat.race._

abstract class Warlock(
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

  val classBranch: String = "Warlock"
  val armorClass: Int = 18
  val actions: Int = 1
  val baseStatsProficiency: Vector[String] = Vector[String]("wis", "cha")
  val abilityCheckProficiency: Vector[String] = Vector[String]("deception", "intimidation")

  protected def spellSave: Int = 8 + this.spellAttack

  protected def spellAttack: Int = this.proficiencyBonus + this.stat("cha")

  protected def eldritchBlast(target: Character): Unit = {
    for (_ <- 0 to ((this.level - 5) / 6) + 1) {
      val atkRoll = this.attackRoll(this.hasAdvantage("range")) + this.spellAttack
      if (this.isSucceed(target, atkRoll)) {
        println(s"A blast struct ${target.name} like a rhino!")
        this.inflictDmg(target, this.diceSet.roll(this.diceSet.d12), "force")
      } else println("A blast beamed ahead like a mad beast, but it missed...")
    }
  }
}
