package com

import java.util.concurrent.Executors

import com.oomagnitude.rx.Observables.{BlockingObservableFactory, ThreadPoolObservableFactory}

package object oomagnitude {
  /**
   * Function that adjusts permanence. Takes in current permanence value as first input, and boolean indicating whether
   * or not the connection overlaps as the second. The function is expected to return the adjusted permanence value.
   */
  type PermanenceFunction = (Double, Boolean) => Double

  /**
   * Size of the sensor. Since we are mapping characters to sensor terminals 1:1, we just need enough to cover the number
   * of characters in the alphabet.
   */
  val SensorSize = 30

  /**
   * Size of the layer. This is the number of coincidence detectors, so in theory, the layer could recognize up to this
   * many letter combinations
   */
  val LayerSize = 2048

  /**
   * Initial model with randomly initialized connections
   */
  val InitialModel = Model(List(Layer.withRandomConnections(LayerSize, SensorSize, connectionProbability = 0.40)),
    new GlobalInhibition(maxWinners = 2))

  /**
   * Creates observables used in parallel execution of layer processing of inputs
   */
  implicit val ObservableFactory = {
    // Compute the number of threads needed for maximum parallelism. Cap at number of available processors
    val numThreads = math.min(Runtime.getRuntime.availableProcessors, InitialModel.layers.size)

    if (numThreads > 1) new ThreadPoolObservableFactory[(Layer, Set[Int]), Layer](Executors.newFixedThreadPool(numThreads))
    else new BlockingObservableFactory[(Layer, Set[Int]), Layer]
  }

  /**
   * Encodings for each letter that may be found in a word. Maps the letter to an index in the sensor.
   */
  val LetterEncodings = ('a' to 'z').toList.zipWithIndex.toMap

  /**
   * Encodes letters in a word as an SDR
   */
  val WordEncoder = new WordEncoder(LetterEncodings)

  /**
   * Reverse mapping of sensor indices to letters
   */
  val IndexToLetter = LetterEncodings.map(_.swap)

  /**
   * Train a model on the word dictionary
   * 
   * @param numIterations the number of iterations to train the word dictionary on
   * @return a model that has been trained on the word dictionary
   */
  def train(numIterations: Int): Model = {
    Stream.continually(Dictionary.Words).flatten.take(numIterations).foldLeft(InitialModel) {
      (model, word) => model.processInput(WordEncoder.encode(word))
    }
  }

  /**
   * Given a set of letters, infer which words contain those letters using the model.
   *
   * @param letters the letters for which to infer words
   * @param model the model containing the layer and poolers
   */
  def inferWord(letters: String, model: Model) = {
    val newModel = model.processInput(WordEncoder.encode(letters))
    val allChars = newModel.layers(0).active.map(c => charsForPooler(c, newModel))
    allChars.foreach { chars =>
      println(); println("matching pooler: " + chars)
      val words = chars.map(Dictionary.ReverseIndex).reduce((a, b) => a.intersect(b)).toList.sortBy(_.length)
      words.foreach { word =>
        word.foreach { char =>
          if (letters.contains(char)) print(Console.RED + char.toString + Console.RESET)
          else print(char.toString)
        }
        println()
      }
    }
  }

  /**
   * Determine which characters a pooler has permanent connections to
   *
   * @param poolerIndex the index for the pooler
   * @param model the model containing the pooler
   * @return all characters in the sensors that the pooler has a permanent connection to
   */
  private def charsForPooler(poolerIndex: Int, model: Model): Iterable[Char] = {
    val sensoryConnections = model.layers(0).poolers(poolerIndex).permanentConnections.keys
    sensoryConnections.flatMap(IndexToLetter.get)
  }
}
