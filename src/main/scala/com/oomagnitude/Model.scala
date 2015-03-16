package com.oomagnitude

import com.oomagnitude.rx.Observables.ObservableFactory

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
  def processInput(input: Set[Int])(implicit observableFactory: ObservableFactory[(Layer, Set[Int]), Layer]): Model = {
    def processLayer(kv: (Layer, Set[Int])): Layer = {
      kv match {
        case (layer, layerInput) =>
          val overlaps = layer.overlap(layerInput)
          val inhibitionWinners = inhibition.compete(overlaps)
          layer.learn(inhibitionWinners, layerInput)
      }
    }

    val jobs = layers.zip(input :: layers.map(_.active))
    val observable = observableFactory.parallel(jobs, processLayer)
    var newLayers = List.empty[Layer]
    observable.toBlocking.foreach{ layer => newLayers = layer :: newLayers}
    this.copy(layers = newLayers)
  }
}
