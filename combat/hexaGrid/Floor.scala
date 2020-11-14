package combat.hexaGrid

import combat.character._

class Floor extends Hexagon {
  private var occupant: Option[Character] = None

  /** Return true if there's no obstacle or character on it. */
  def isEmpty: Boolean = this.occupant.isEmpty

  /** Return the Character currently on the hexagon. */
  def character: Option[Character] = this.occupant

  /** Return true if there is a character or an obstacle here.
   *
   * In a more sophisticate setup, the hexagon with character
   * should still be passable, just not stayable. */
  def isUnpassable: Boolean = false

  /** Clear content. */
  def clear(): Unit = this.occupant = None

  /** Add a character on to the hexagon. Return true if the place was empty
   * and false otherwise */
  def addCharacter(figure: Character): Boolean = {
    if (this.isEmpty) {
      this.occupant = Some(figure)
      true
    } else false
  }
}
