package combat.race

trait Undead extends Race {
  override val dmgImmunity: Vector[String] = Vector[String]("necrotic", "poison")

  override val dmgResistant: Vector[String] = Vector[String]("normal")

  override def toString: String = "Undead"
}
