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

  protected def eldritchBlast(target: Character): String = {
    var actionString = ""
    for (_ <- 0 to ((this.level - 5) / 6) + 1) {
      val atkDice = this.attackRoll(this.hasAdvantage("range"))
      val atkRoll = atkDice + this.spellAttack
      println(s"Attack roll: $atkRoll")
      if (this.isSucceed(target, atkRoll)) {
        this.inflictDmg(target, this.diceSet.roll(this.diceSet.d12) + this.meleeOrRange("range") + this.isCriticalHit(atkDice, this.diceSet.d12), "force")
        actionString += s"A blast struct ${target.name} like a rhino!"
      } else actionString += "A blast beamed ahead like a mad beast, but it missed..."
    }
    actionString
  }

  protected def tollTheDead(target: Character): String = {
    val loudness = ((this.level - 5) / 6) + 1
    val saving = target.savingThrow("wis")
    val dmgDice = if (!target.isFullHp) this.diceSet.d12 else this.diceSet.d10
    if (this.spellSave > saving) {
      this.inflictDmg(target, this.diceSet.roll(loudness)(dmgDice), "necrotic")
      s"${this.name} pointed at ${target.name}, ${target.name} heard a disgusting sound coming from an unknown source, it's unbearable!"
    } else s"${this.name} pointed at ${target.name}, ${target.name} grinned as if the attack was anticipated..."
  }
}
