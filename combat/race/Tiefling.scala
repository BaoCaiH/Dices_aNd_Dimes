package combat.race

class Tiefling extends Race {
  override val dmgResistant: Vector[String] = Vector[String]("fire")

  override def toString: String = "Tiefling"
}
