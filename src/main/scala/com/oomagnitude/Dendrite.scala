package com.oomagnitude

/**
 * Amount of overlap that a dendrite has with a particular input
 *
 * @param dendriteIndex the index of the dendrite
 * @param permanentOverlap the number of permanent synapses in the dendrite that overlap with the input
 * @param weightedOverlap the weighted overlap of synapses on the dendrite, where each weight is the permanence
 */
case class Overlap(dendriteIndex: Int, permanentOverlap: Int, weightedOverlap: Double)

/**
 * Construct for a single coincidence detector (pooler), known as a dendrite. A dendrite has a location (index) and
 * a set of synapses which connect it to the sensor
 *
 * @param index the dendrite's index
 * @param synapses the synapses on the dendrite, where the map key is the index of the sensor that this dendrite connects
 *                 to, and the map value is the permanence of the synapse
 * @param permanenceThreshold the threshold for considering a synapse permanent (i.e., connected). If permanence is below
 *                            this number, the synapse is not considered connection and thus does not contribute to
 *                            overlap.
 * @param updatePermanence a function that modifies a synapse permanence value during learning
 */
case class Dendrite(index: Int, synapses: Map[Int, Double],
                    permanenceThreshold: Double,
                    updatePermanence: PermanenceFunction) {
  /**
   * Contains only the synapses that are permanent (i.e., connected)
   */
  lazy val permanentSynapses = synapses.filter(kv => kv._2 >= permanenceThreshold)

  /**
   * Apply Hebbian learning by adjusting permanence values of synapses on this dendrite up or down, depending on whether
   * the synapse is overlapping with the input.
   *
   * @param input the input to reinforce
   * @return the new dendrite with permanence values adjusted
   */
  def learn(input: Set[Int]): Dendrite = {
    this.copy(synapses = synapses.map {
      case (axonIndex, permanence) =>
        axonIndex -> updatePermanence(permanence, input.contains(axonIndex))
    })
  }

  /**
   * Compute the amount of overlap that this dendrite has with an input set
   *
   * @param input the input to compute overlap for
   * @return the overlap that this dendrite has to the synapse
   */
  def overlap(input: Set[Int]): Overlap = {
    // Filter down to only synapses that overlap with the input
    val permanentOverlap = permanentSynapses.filterKeys(input.contains)
    // Compute the weighted overlap, which takes into account the synapse permanence values as well as the ratio of
    // overlapping to total permanent synapses on the dendrite
    val weightedOverlap =
      if (permanentSynapses.nonEmpty) permanentOverlap.values.sum * permanentOverlap.size / permanentSynapses.size
      else 0.0
    Overlap(index, permanentOverlap = permanentOverlap.size, weightedOverlap = weightedOverlap)
  }

}