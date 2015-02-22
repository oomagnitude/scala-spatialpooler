package com.oomagnitude

import com.oomagnitude.geometry.Geometry
import com.oomagnitude.layer.{GlobalInhibition, Layer}

import scala.util.Random

object Main extends App {
  val SensorGeometry = Geometry(32,32)
  val LayerGeometry = Geometry(16,16)
  val InitialModel = Model(Layer.withRandomConnections(LayerGeometry, SensorGeometry, connectionProbability = 0.15),
    new GlobalInhibition(maxWinners = 5), List.empty)

  val random = new Random()
  val LetterEncodings = ('a' to 'z').zip(random.shuffle(SensorGeometry.coordinates).sliding(2,2).map(_.toSet).toIterable).toMap

  var model = InitialModel
  var iteration = 1
  for (word <- Dictionary.Words) {
    val uniqueChars = word.toCharArray.toSet
    val input = uniqueChars.flatMap(char => LetterEncodings.getOrElse(char, Set.empty))
    model = model.processInput(input)
    if (iteration % 1000 == 0) {
      println(s"$iteration: ${model.winners}")
    }
    iteration += 1
  }
}
