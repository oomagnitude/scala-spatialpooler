package com.oomagnitude

case class Model(layer: Layer, inhibition: Inhibition, winners: Iterable[Int]) {
  def processInput(input: Set[Int]): Model = {
    val overlaps = layer.overlap(input)
    val inhibitionWinners = inhibition.compete(overlaps)
    this.copy(layer = layer.learn(inhibitionWinners, input), winners = inhibitionWinners)
  }
}
