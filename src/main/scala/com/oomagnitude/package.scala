package com

import com.oomagnitude.geometry.{Coordinate, Geometry}
import com.oomagnitude.layer.{GlobalInhibition, Layer}

import scala.util.Random

package object oomagnitude {
  type PermanenceFunction = (Double, Boolean) => Double

  val SensorGeometry = Geometry(10,10)
  val LayerGeometry = Geometry(32,32)
  val InitialModel = Model(Layer.withRandomConnections(LayerGeometry, SensorGeometry, connectionProbability = 0.20),
    new GlobalInhibition(maxWinners = 5), List.empty)
  val random = new Random()
  val LetterEncodings = ('a' to 'z').zip(random.shuffle(SensorGeometry.coordinates)).toMap
  val CoordinateToLetter = LetterEncodings.map(_.swap)

  def train: Model = {
    var model = InitialModel
    for {
      i <- 1 to 20
      word <- Dictionary.Words
    } {
      val input = sdrForWord(word)
      model = model.processInput(input)
    }
    model
  }

  private def sdrForWord(word: String): Set[Coordinate] = {
    val uniqueChars = word.toLowerCase.toCharArray.toSet
    uniqueChars.collect {case char if LetterEncodings.contains(char) => LetterEncodings(char)}
  }

  def inferWord(word: String, model: Model) = {
    val newModel = model.processInput(sdrForWord(word))
    val allChars = newModel.winners.flatMap(c => charsForDendrite(c, newModel)).toSet
    val words = allChars.map(Dictionary.ReverseIndex).reduce((a, b) => a.intersect(b))
    words.foreach(println)
  }

  private def charsForDendrite(dendriteAddress: Coordinate, model: Model): Iterable[Char] = {
    model.layer.dendrites(dendriteAddress).permanentSynapses.keys.map(CoordinateToLetter)
  }
}
