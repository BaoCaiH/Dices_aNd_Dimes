package combat.hexaGrid

import combat.character.Character

import scala.collection.mutable

class HexaGrid(val width: Int, val height: Int) {
  val size: Int = (width + 1) * (height / 2) + (height % 2) * ((width / 2) + 1)

  private def placeHexagon(x: Int, y: Int): Hexagon = {
    if (x <= 1 || x >= this.height - 2 || y == 0 || y == this.width - 1) Obstacle
    else new Floor
  }

  /** Initialize a hexagon map, a mutable one.
   *
   * Different from conventional square map,
   * the rule for initialization is also different. */
  private def initialElements: mutable.Map[Int, mutable.Map[Int, Hexagon]] = {
    var map = mutable.Map[Int, mutable.Map[Int, Hexagon]]()
    for (x <- 0 until this.width) {
      var innerMap = mutable.Map[Int, Hexagon]()
      for (y <- 0 until this.height) {
        if (x % 2 == y % 2) innerMap += (y -> this.placeHexagon(x, y))
      }
      map += (x -> innerMap)
    }

    map
  }

  private val contents: mutable.Map[Int, mutable.Map[Int, Hexagon]] = this.initialElements

  private def contains(x: Int, y: Int): Boolean =
    this.contents.contains(x) && this.contents(x).contains(y)

  /** Check if the coordinate exist in the hexagon map.
   *
   * Again since the hexagon map works differently
   * from the traditional square map, not all the
   * "within the range" coordinates work.
   *
   * @param position position on the grid */
  def contains(position: HexaGridPos): Boolean =
    this.contains(position.x, position.y)


  /** Return the element at the expected coordinates.
   *
   * @param position position on the grid */
  def elementAt(position: HexaGridPos): Hexagon = {
    require(this.contains(position), "This position does not exist in the hexagon grid.")
    this.contents(position.x)(position.y)
  }

  /** Same as elementAt.
   *
   * @param position position on the grid */
  def apply(position: HexaGridPos): Hexagon = this.elementAt(position)

  /** Update or rather change the current element at the given coordinate
   * with the new given element.
   *
   * @param position   position on the grid
   * @param newElement new element to be put here */
  def update(position: HexaGridPos, newElement: Hexagon): Unit = {
    require(this.contains(position), "Attempted to update a non-existent hexagon.")
    this.contents(position.x)(position.y) = newElement
  }

  def possibleElementAt(position: HexaGridPos): Option[Hexagon] =
    if (this.contains(position)) Some(this (position)) else None


  /** Return a vector of elements surrounding the given element.
   *
   * Filter out those that are out of the grid.
   *
   * @param position the location to check for neighbors */
  def neighbors(position: HexaGridPos): Vector[HexaGridPos] = {
    require(this.contains(position), "This position does not exist in the hexagon grid.")
    position.neighbors.filter(this.contains)
  }

  /** Return all the positions in this HexaGrid. */
  def allPositions: Vector[HexaGridPos] = {
    (
      for (
        x <- this.contents;
        y <- x._2
      ) yield HexaGridPos(x._1, y._1)
      ).toVector
  }

  /** Return all hexagon on the HexaGrid. */
  def allHexagons: Vector[Hexagon] = {
    for (
      position <- this.allPositions;
      hexa <- this.possibleElementAt(position)
    ) yield hexa
  }

  /** Swap contents at 2 positions.
   *
   * @param position1 first position to swap
   * @param position2 swap content on this position with the first */
  def swap(position1: HexaGridPos, position2: HexaGridPos): Unit = {
    require(this.contains(position1), "First position does not exist in the hexagon grid.")
    require(this.contains(position2), "Second position does not exist in the hexagon grid.")
    val temp = this (position1)
    this (position1) = this (position2)
    this (position2) = temp
  }

  def allCharacters: Vector[Character] = {
    this.allHexagons
      .filter(_.character.isDefined)
      .map(_.character.get)
  }

  override def toString: String = s"Board of width of ${this.width} and height of ${this.height} units."
}
