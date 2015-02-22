package com

import scala.util.Random

package object oomagnitude {
  type PermanenceFunction = (Double, Boolean) => Double

  val SensorSize = 30
  val LayerSize = 1024
  val InitialModel = Model(Layer.withRandomConnections(LayerSize, SensorSize, connectionProbability = 0.20),
    new GlobalInhibition(maxWinners = 1), List.empty)
  val random = new Random()
  val LetterEncodings = ('a' to 'z').zip(random.shuffle((0 until SensorSize).toList)).toMap
  val CoordinateToLetter = LetterEncodings.map(_.swap)

  def train: Model = {
    var model = InitialModel
    for {
      i <- 1 to 20
      word <- Dictionary.Words
    } {model = model.processInput(sdrForWord(word))}
    model
  }

  private def sdrForWord(word: String): Set[Int] = {
    val uniqueChars = word.toLowerCase.toCharArray.toSet
    uniqueChars.collect {case char if LetterEncodings.contains(char) => LetterEncodings(char)}
  }

  def inferWord(word: String, model: Model) = {
    val newModel = model.processInput(sdrForWord(word))
    val allChars = newModel.winners.map(c => charsForDendrite(c, newModel))
    val words = allChars.flatMap(cs => cs.map(Dictionary.ReverseIndex).reduce((a, b) => a.intersect(b))).toSet
    words.toList.sortBy(_.length).foreach(println)
  }

  private def charsForDendrite(dendriteIndex: Int, model: Model): Iterable[Char] = {
    model.layer.dendrites(dendriteIndex).permanentSynapses.keys.map(CoordinateToLetter)
  }
}
