package combat.character

import combat.hexaGrid.{HexaGrid, HexaGridPos}
import combat.race._

abstract class Bard(
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

  val classBranch: String = "Bard"
  val armorClass: Int = 18
  val actions: Int = 1
  val baseStatsProficiency: Vector[String] = Vector[String]("dex", "cha")
  val abilityCheckProficiency: Vector[String] = Vector[String]("acrobatic", "deception", "performance")
  protected var healing: Int = 10

  /** Return help message tailored for classes and races. */
  override def helpMessage: String = {
    val common = super.helpMessage
    common + "\n" +
      "Bard's specific commands\n" +
      "\taction 1 [target's name]: whisper at a target and make it go mad\n"
  }

  protected def spellAttack: Int = this.proficiencyBonus + this.stat("cha")

  protected def spellSave: Int = 8 + this.spellAttack

  //  /** This spell wrack the target with pain. Target must make a Wisdom saving throw.
  //   * On a failed save, target takes 5d6 psychic damage.
  //   *
  //   * @param target a character. */
  protected def dissonantWhisper(target: Character): String = {
    val saving = target.savingThrow("wis")
    if (this.spellSave > saving) {
      this.inflictDmg(target, this.diceSet.roll(5)(this.diceSet.d6), "psychic")
      s"${this.name} pointed at ${target.name}, ${target.name} heard a terrible sound coming from an unknown source, it's painful!\n"
    } else s"${this.name} pointed at ${target.name}, ${target.name} grinned as if the attack was anticipated...\n"
  }

}
