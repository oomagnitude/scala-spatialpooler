package com.oomagnitude

/**
 * Amount of overlap that a pooler has with a particular input
 *
 * @param poolerIndex the index of the pooler
 * @param weightedOverlap the total number of permanent connections overlapping the input, weighted by the ratio of
 *                        matching to total number of permanent connections.
 */
case class Overlap(poolerIndex: Int, weightedOverlap: Double)

/**
 * Construct for a single coincidence detector (pooler). A pooler has a location (index) and
 * a set of connections which connect it to the sensor
 *
 * @param index the pooler's index
 * @param connections the connections on the pooler, where the map key is the index of the sensor that this pooler connects
 *                 to, and the map value is the permanence of the connection
 * @param permanenceThreshold the threshold for considering a connection permanent (i.e., connected). If permanence is
 *                            below this number, the connection does not contribute to overlap.
 * @param updatePermanence a function that modifies a permanence value during learning
 */
case class Pooler(index: Int, connections: Map[Int, Double], permanenceThreshold: Double, updatePermanence: PermanenceFunction) {
  /**
   * Contains only the connections that are permanent (i.e., connected)
   */
  lazy val permanentConnections = connections.filter(kv => kv._2 >= permanenceThreshold)

  /**
   * Apply Hebbian learning by adjusting permanence values of connections on this pooler up or down, depending on whether
   * the connection is overlapping with the input.
   *
   * @param input the input to reinforce
   * @return the new pooler with permanence values adjusted
   */
  def learn(input: Set[Int]): Pooler = {
    this.copy(connections = connections.map {
      case (axonIndex, permanence) =>
        axonIndex -> updatePermanence(permanence, input.contains(axonIndex))
    })
  }

  /**
   * Compute the amount of overlap that this pooler has with an input set
   *
   * @param input the input to compute overlap for
   * @return the overlap that this pooler has to the input
   */
  def overlap(input: Set[Int]): Overlap = {
    // Count permanent connections that overlap with the input
    val permanentOverlap = permanentConnections.count(kv => input.contains(kv._1))
    // Compute the weighted overlap, which takes into account the permanence values as well as the ratio of
    // overlapping to total permanent connections on the pooler
    val weightedOverlap =
      if (permanentConnections.nonEmpty) permanentOverlap * permanentOverlap / permanentConnections.size
      else 0.0
    Overlap(index, weightedOverlap = weightedOverlap)
  }

}