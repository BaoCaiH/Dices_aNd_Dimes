package combat.hexaGrid

import combat.character.constants.boostedStatsBonus

object Booster extends Floor {
  private var boost = 1

  def boostCharacter(): Boolean = {
    if (this.character.isDefined && this.boost > 0) {
      this.boost -= 1
      this.character.get.temporaryStatsBonus = boostedStatsBonus ++ this.character.get.temporaryStatsBonus
      true
    } else false
  }
}
