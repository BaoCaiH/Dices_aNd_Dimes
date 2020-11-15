package combat.race

trait Tabaxi extends Race {
  private var felineCapability: Int = 1

  override val abilityCheckProficiency: Vector[String] = Vector[String]("perception", "stealth")

  def felineAgility(): Unit = {
    if (felineCapability > 0) {
      this.bonusSpeed = this.speed
      this.felineCapability -= 1
    }
  }

  override def toString: String = "Tabaxi"
}
