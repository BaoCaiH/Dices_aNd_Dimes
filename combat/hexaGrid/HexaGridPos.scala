package combat.hexaGrid

case class HexaGridPos(x: Int, y: Int) extends Product with Serializable {

  /** Returns the 6 neighboring hexagons, no exception. */
  def neighbors: Vector[HexaGridPos] = {
    Vector(
      HexaGridPos(this.x - 2, this.y),
      HexaGridPos(this.x - 1, this.y + 1),
      HexaGridPos(this.x + 1, this.y + 1),
      HexaGridPos(this.x + 2, this.y),
      HexaGridPos(this.x + 1, this.y - 1),
      HexaGridPos(this.x - 1, this.y - 1)
    )
  }

  /** Compare with another position.
   *
   * They are considered the same as long as
   * the coordinates are the same.
   *
   * @param another another position */
  def ==(another: HexaGridPos): Boolean = this.x == another.x && this.y == another.y

  /** Return the difference on the x-direction.
   *
   * This is only artificial, on a hexagon board,
   * there is no true direction. Unless it's
   * radian or degree, but we don't do that here.
   *
   * @param another another position */
  def xDiff(another: HexaGridPos): Int = another.x - this.x

  /** Return the difference on the y-direction.
   *
   * This is only artificial, on a hexagon board,
   * there is no true direction. Unless it's
   * radian or degree, but we don't do that here.
   *
   * @param another another position */
  def yDiff(another: HexaGridPos): Int = another.y - this.y

  /** Return the difference on the x and y-direction.
   *
   * This is only artificial, on a hexagon board,
   * there is no true direction. Unless it's
   * radian or degree, but we don't do that here.
   *
   * @param another another position */
  def diff(another: HexaGridPos): (Int, Int) = (this.xDiff(another), this.yDiff(another))

  /** Return the distance to another position.
   *
   * Movement on the hexagon board is unique.
   * One move sideway causes the verticle direction
   * to change by 1 unit as well. Meanwhile,
   * moving in the verticle direction is particularly easy.
   * It only takes half the distance unit to travel
   * a given y-diff, i.e. 2 steps to traverse 4 units
   * vertically.
   *
   * @param another another position */
  def distance(another: HexaGridPos): Int = {
    val xDiff = this.xDiff(another).abs
    val yDiff = this.yDiff(another).abs
    yDiff + math.max(0, (xDiff - yDiff) / 2)
  }

  override def toString: String = s"($x, $y)"
}
