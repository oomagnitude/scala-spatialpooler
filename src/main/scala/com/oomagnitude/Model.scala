package com.oomagnitude

import com.oomagnitude.geometry.Coordinate
import com.oomagnitude.layer.{Inhibition, Layer}

case class Model(layer: Layer, inhibition: Inhibition, winners: Iterable[Coordinate]) {
  def processInput(input: Set[Coordinate]): Model = {
    val overlaps = layer.overlap(input)
    val inhibitionWinners = inhibition.compete(overlaps)
    this.copy(layer = layer.learn(inhibitionWinners, input), winners = inhibitionWinners)
  }
}
