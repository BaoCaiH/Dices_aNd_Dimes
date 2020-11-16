package combat.race

class Elf extends Race {
  override val speed: Int = 35

  override val abilityCheckProficiency: Vector[String] = Vector[String]("perception")

  override def toString: String = "Elf"
}
