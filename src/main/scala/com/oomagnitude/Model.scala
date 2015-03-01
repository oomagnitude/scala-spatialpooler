package com.oomagnitude

/**
 * Construct for a single-layer model
 *
 * @param layer the layer in the model
 * @param inhibition the inhibition strategy
 * @param winners the winners of the latest inhibition race
 */
case class Model(layer: Layer, inhibition: Inhibition, winners: Iterable[Int]) {

  /**
   * Process an input from the layer's sensor by:
   * 1. Computing overlap to input
   * 2. Running the inhibition race
   * 3. Learning the input from winning poolers
   *
   * @param input the input to process
   * @return the new model state, after inhibition
   */
  def processInput(input: Set[Int]): Model = {
    val overlaps = layer.overlap(input)
    val inhibitionWinners = inhibition.compete(overlaps)
    this.copy(layer = layer.learn(inhibitionWinners, input), winners = inhibitionWinners)
  }
}
