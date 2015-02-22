package com.oomagnitude.geometry

/**
 * A collection of elements that can be randomly accessed by a [[Coordinate]]
 * @tparam E the type of element
 */
case class Grid[E](geometry: Geometry, elements: IndexedSeq[E]) {

  /**
   * look up an element
   * @param coordinate the element's coordinate
   * @return the element
   */
  def apply(coordinate: Coordinate): E = elements(geometry.indexForCoordinate(coordinate))

  /**
   * Update this grid at the specified coordinate by replacing the element there with a new one
   *
   * @param coordinate the address to make the update at
   * @param element the element to replace
   * @return the grid with the element replaced
   */
  def updated(coordinate: Coordinate, element: E): Grid[E] = {
    this.copy(elements = elements.updated(geometry.indexForCoordinate(coordinate), element))
  }

  /**
   * update the elements vector with an list of addresses to change, plus a function to call in order to calculate the
   * new value.
   * @param coordinates the addresses in the elements vector to change, plus the argument to deliver to the update
   *                  function for each
   * @param updateFunction the update function (receives the previous value and the argument for that address, and
   *                       returns the new value). By default, the value at A is replaced with ARG.
   * @tparam T the type of the argument
   * @return a new grid instance with the updated grid
   */
  def updated[T](coordinates: Iterable[(Coordinate, T)], updateFunction: (E, T) => E): Grid[E] = {
    coordinates.foldLeft(this) {
      case (grid, (coordinate, argument)) =>
        grid.updated(coordinate, updateFunction(grid(coordinate), argument))
    }
  }

  private def replaceFunction[E]: (E,E) => E = (oldValue: E, update: E) => update

  /**
   * replace the given elements with the elements in the list
   * @param coordinates the list of updates
   * @return a new grid instance with the updated grid
   */
  def updated(coordinates: Iterable[(Coordinate, E)]): Grid[E] = updated(coordinates, replaceFunction[E])

}

object Grid {

  /**
   * create a grid of elements
   * @param geometry the geometry of the grid (the index type, really)
   * @param elementFactory a function to create elements
   * @tparam E the type of the elements
   * @return a new grid
   */
  def create[E](geometry: Geometry, elementFactory: Coordinate => E): Grid[E] = {
    val elements = geometry.coordinates.map(c => elementFactory(c))
    Grid[E](geometry, elements)
  }

}