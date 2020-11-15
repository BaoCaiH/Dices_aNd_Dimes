package combat.race

trait Halfling extends Race {
  override val speed: Int = 25

  override def toString: String = "Halfling"
}
