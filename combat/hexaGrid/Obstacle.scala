package combat.hexaGrid

import combat.character._

object Obstacle extends Hexagon {
  val isEmpty = false

  val character: Option[Character] = None

  val isUnpassable = true

  def clear(): Unit = {}

  def addCharacter(figure: Character): Boolean = false
}
