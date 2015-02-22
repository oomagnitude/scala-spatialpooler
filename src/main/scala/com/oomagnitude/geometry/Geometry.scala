package com.oomagnitude.geometry

case class Geometry(height: Int, width: Int) {
  val coordinates: IndexedSeq[Coordinate] = (for {
    i <- 0 until width
    j <- 0 until height
  } yield Coordinate(i, j)).toVector.sortBy(indexForCoordinate)

  /**
   * A scalar index for a coordinate in the context of this geometry.
   * Index 0 is at the bottom left corner (0, 0), index 1 is one to the right.
   * @param c the coordinate
   * @return a scalar index
   */
  def indexForCoordinate(c: Coordinate): Int = {
    require(includes(c), s"coordinate ($c) must be within this geometry ($this)")
    c.y * this.width + c.x
  }

  def includes(c: Coordinate): Boolean = {
    c.x >= 0 && c.x < width && c.y >= 0 && c.y < height
  }

}

case class Coordinate(x: Int, y: Int) {
  override def toString = s"$x:$y"
}
