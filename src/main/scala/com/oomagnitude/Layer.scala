package com.oomagnitude

import scala.util.Random

object Layer {
  /**
   * The minimum value a synapse permanence can be before it is considered connected
   */
  val PermanenceThreshold = 1.0

  /**
   * Function that adjusts synapse permanence value up or down, depending on whether the synapse has input
   */
  val adjustPermanence: PermanenceFunction = {(permanence, isOverlapping) =>
    val sign = if (isOverlapping) 1.0 else -1.0
    permanence + 0.1 * sign
  }

  /**
   * Construct a layer with randomly initialized connections
   *
   * @param layerSize the size of the layer
   * @param sensorSize the size of the input (sensor)
   * @param connectionProbability the probability that any given dendrite in the layer will connect to any given input
   *                              from the sensor
   * @return the fully constructed layer
   */
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

/**
 * Construct for an ensemble of dendrites that mutually compete in the inhibition race
 *
 * @param dendrites the dendrites in this layer
 */
case class Layer(dendrites: IndexedSeq[Dendrite]) {

  /**
   * Compute overlaps for all dendrites in this layer
   *
   * @param input the input to compute overlap for
   * @return the overlaps for all dendrites with at least one permanent synapse connected to the input
   */
  def overlap(input: Set[Int]): Iterable[Overlap] = {
    dendrites.map {
      dendrite =>
        dendrite.overlap(input)
    }.filter(_.permanentOverlap > 0)
  }

  /**
   * Reinforce the input on qualifying dendrites
   *
   * @param learners the dendrites that will learn
   * @param input the input to learn
   * @return the new layer state after learning
   */
  def learn(learners: Iterable[Int], input: Set[Int]): Layer = {
    this.copy(dendrites = learners.foldLeft(dendrites) {
      (ds, dendriteIndex) =>
        ds.updated(dendriteIndex, ds(dendriteIndex).learn(input))
    })
  }
}
