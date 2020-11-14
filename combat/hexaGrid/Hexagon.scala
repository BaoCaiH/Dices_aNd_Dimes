package combat.hexaGrid

import combat.character._

abstract class Hexagon {

  /** Return true if there's no obstacle or character on it. */
  def isEmpty: Boolean

  /** Opposite of isEmpty, just here for convenience. */
  def nonEmpty: Boolean = !this.isEmpty

  /** Return the Character currently on the hexagon. */
  def character: Option[Character]

  /** Return true if there is a character or an obstacle here.
   *
   * In a more sophisticate setup, the hexagon with character
   * should still be passable, just not stayable. */
  def isUnpassable: Boolean

  /** Clear content. */
  def clear(): Unit

  /** Add a character on to the hexagon. Return true if the place was empty
   * and false otherwise */
  def addCharacter(figure: Character): Boolean
}
