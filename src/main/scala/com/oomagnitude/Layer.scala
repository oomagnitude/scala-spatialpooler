package com.oomagnitude

import scala.util.Random

object Layer {
  /**
   * The minimum value a connection's permanence can be before it is considered connected
   */
  val PermanenceThreshold = 1.0

  /**
   * Function that adjusts permanence value up or down, depending on whether the connection has input
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
   * @param connectionProbability the probability that any given pooler in the layer will connect to any given input
   *                              from the sensor
   * @return the fully constructed layer
   */
  def withRandomConnections(layerSize: Int, sensorSize: Int, connectionProbability: Double): Layer = {
    val random = new Random()
    val connectionsPerPooler = (sensorSize * connectionProbability).toInt

    def initialConnections: Map[Int, Double] = {
      random.shuffle((0 until sensorSize).toList).take(connectionsPerPooler).map {
        c => c -> (PermanenceThreshold + random.nextGaussian())
      }.toMap
    }

    val poolers = for {
      index <- 0 until layerSize
    } yield Pooler(index, initialConnections, PermanenceThreshold, adjustPermanence)

    Layer(poolers, Set.empty)
  }
}

/**
 * Construct for an ensemble of poolers that mutually compete in the inhibition race
 *
 * @param poolers the poolers in this layer
 * @param active the poolers that are presently active. These are the ones that won the latest inhibition race
 */
case class Layer(poolers: IndexedSeq[Pooler], active: Set[Int]) {

  /**
   * Compute overlaps for all poolers in this layer
   *
   * @param input the input to compute overlap for
   * @return the overlaps for all poolers with at least one permanent connection to the input
   */
  def overlap(input: Set[Int]): Iterable[Overlap] = {
    poolers.map {
      pooler =>
        pooler.overlap(input)
    }.filter(_.permanentOverlap > 0)
  }

  /**
   * Reinforce the input on qualifying poolers
   *
   * @param learners the poolers that will learn
   * @param input the input to learn
   * @return the new layer state after learning
   */
  def learn(learners: Set[Int], input: Set[Int]): Layer = {
    this.copy(active = learners, poolers = learners.foldLeft(poolers) {
      (ps, poolerIndex) =>
        ps.updated(poolerIndex, ps(poolerIndex).learn(input))
    })
  }
}
