package combat.race

trait Race {

  val speed: Int = 30

  var bonusSpeed: Int = 0

  val dmgImmunity: Vector[String] = Vector[String]()

  val dmgResistant: Vector[String] = Vector[String]()

  val abilityCheckProficiency: Vector[String] = Vector[String]()

  def helpMessage: String = ""

  override def toString: String = "Outcast"
}
