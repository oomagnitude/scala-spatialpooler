package com.oomagnitude.layer

import com.oomagnitude.PermanenceFunction
import com.oomagnitude.dendrite.{Overlap, Dendrite}
import com.oomagnitude.geometry.{Geometry, Coordinate, Grid}

import scala.util.Random

object Layer {
  val PermanenceThreshold = 1.0
  val InitialPermanence = 0.7

  val adjustPermanence: PermanenceFunction = {(permanence, isOverlapping) =>
    val sign = if (isOverlapping) 1.0 else -1.0
    permanence + 0.1 * sign
  }


  def withRandomConnections(layerGeometry: Geometry, sensorGeometry: Geometry, connectionProbability: Double): Layer = {
    val random = new Random()
    val connectionsPerDendrite = (sensorGeometry.coordinates.size * connectionProbability).toInt

    def initialConnections: Map[Coordinate, Double] = {
      random.shuffle(sensorGeometry.coordinates).take(connectionsPerDendrite).map(c => c -> InitialPermanence).toMap
    }

    val dendrites = Grid.create(layerGeometry, {coordinate => Dendrite(coordinate, initialConnections, PermanenceThreshold, adjustPermanence)})

    Layer(dendrites)
  }
}

case class Layer(dendrites: Grid[Dendrite]) {
  def overlap(input: Set[Coordinate]): Iterable[Overlap] = {
    dendrites.elements.map {
      dendrite =>
        dendrite.overlap(input)
    }.filter(_.rawOverlap > 0)
  }

  def learn(learners: Iterable[Coordinate], input: Set[Coordinate]): Layer = {
    this.copy(dendrites =
      dendrites.updated(learners.map(dendriteAddress => (dendriteAddress, dendrites(dendriteAddress).learn(input)))))
  }
}
