package combat.race

class Tabaxi extends Race {
  private var felineCapability: Int = 1

  override val abilityCheckProficiency: Vector[String] = Vector[String]("perception", "stealth")

  def felineAgility(): String = {
    if (felineCapability > 0) {
      this.bonusSpeed = this.speed
      this.felineCapability -= 1
      s"The cat went meow and ready to leap forward."
    } else {
      s"The cat is tired."
    }
  }

  override def helpMessage: String = {
    val common = super.helpMessage
    common + "\n" +
      "Tabaxi's specific commands\n" +
      "\tfeline agility: additional speed to this turn\n"
  }

  override def toString: String = "Tabaxi"
}
