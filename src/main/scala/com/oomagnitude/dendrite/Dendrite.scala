package com.oomagnitude.dendrite

import com.oomagnitude.PermanenceFunction
import com.oomagnitude.geometry.Coordinate

case class Overlap(address: Coordinate, permanentOverlap: Int, rawOverlap: Int)

case class Dendrite(address: Coordinate, synapses: Map[Coordinate, Double],
                    permanenceThreshold: Double,
                    updatePermanence: PermanenceFunction) {
  lazy val (permanentSynapses, nonPermanentSynapses) = synapses.partition(kv => kv._2 >= permanenceThreshold)

  def learn(input: Set[Coordinate]): Dendrite = {
    this.copy(synapses = synapses.map {
      case (axonAddress, permanence) =>
        axonAddress -> updatePermanence(permanence, input.contains(axonAddress))
    })
  }

  def overlap(input: Set[Coordinate]) = {
    Overlap(address, permanentSynapses.count(kv => input.contains(kv._1)), synapses.count(kv => input.contains(kv._1)))
  }

}