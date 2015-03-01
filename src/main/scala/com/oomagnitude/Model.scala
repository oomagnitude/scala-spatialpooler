package com.oomagnitude

/**
 * Construct for a hierarchy of layers stacked vertically on top of one another
 *
 * @param layers the layers in the model
 * @param inhibition the inhibition strategy
 */
case class Model(layers: List[Layer], inhibition: Inhibition) {

  /**
   * Process an input from each layer's sensor by:
   * 1. Computing overlap to input
   * 2. Running the inhibition race
   * 3. Learning the input from winning poolers
   *
   * @param input the input to process
   * @return the new model state, after inhibition
   */
  def processInput(input: Set[Int]): Model = {
    var layerInput = input
    this.copy(layers = layers.map {
      layer =>
        val overlaps = layer.overlap(layerInput)
        val inhibitionWinners = inhibition.compete(overlaps)
        val newLayer = layer.learn(inhibitionWinners, layerInput)
        
        layerInput = inhibitionWinners
        newLayer
    })
  }
}
