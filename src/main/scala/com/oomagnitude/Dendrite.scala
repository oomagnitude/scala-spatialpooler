package com.oomagnitude

case class Overlap(dendriteIndex: Int, permanentOverlap: Int, rawOverlap: Int, weightedOverlap: Double)

case class Dendrite(index: Int, synapses: Map[Int, Double],
                    permanenceThreshold: Double,
                    updatePermanence: PermanenceFunction) {
  lazy val (permanentSynapses, nonPermanentSynapses) = synapses.partition(kv => kv._2 >= permanenceThreshold)

  def learn(input: Set[Int]): Dendrite = {
    this.copy(synapses = synapses.map {
      case (axonIndex, permanence) =>
        axonIndex -> updatePermanence(permanence, input.contains(axonIndex))
    })
  }

  def overlap(input: Set[Int]) = {
    val permanentOverlap = permanentSynapses.filterKeys(input.contains)
    Overlap(index, permanentOverlap = permanentOverlap.size,
      rawOverlap = synapses.count(kv => input.contains(kv._1)),
      weightedOverlap = permanentOverlap.values.sum)
  }

}