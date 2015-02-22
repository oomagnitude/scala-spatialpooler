package com.oomagnitude

import scala.util.Random

object Layer {
  val PermanenceThreshold = 1.0

  val adjustPermanence: PermanenceFunction = {(permanence, isOverlapping) =>
    val sign = if (isOverlapping) 1.0 else -1.0
    permanence + 0.1 * sign
  }

  def withRandomConnections(layerSize: Int, sensorSize: Int, connectionProbability: Double): Layer = {
    val random = new Random()
    val connectionsPerDendrite = (sensorSize * connectionProbability).toInt

    def initialConnections: Map[Int, Double] = {
      random.shuffle((0 until sensorSize).toList).take(connectionsPerDendrite).map {
        c => c -> (PermanenceThreshold + random.nextGaussian())
      }.toMap
    }

    val dendrites = for {
      index <- 0 until layerSize
    } yield Dendrite(index, initialConnections, PermanenceThreshold, adjustPermanence)

    Layer(dendrites)
  }
}

case class Layer(dendrites: IndexedSeq[Dendrite]) {
  def overlap(input: Set[Int]): Iterable[Overlap] = {
    dendrites.map {
      dendrite =>
        dendrite.overlap(input)
    }.filter(_.rawOverlap > 0)
  }

  def learn(learners: Iterable[Int], input: Set[Int]): Layer = {
    this.copy(dendrites = learners.foldLeft(dendrites) {
      (ds, dendriteIndex) =>
        ds.updated(dendriteIndex, ds(dendriteIndex).learn(input))
    })
  }
}
