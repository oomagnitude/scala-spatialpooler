package com.oomagnitude

import java.nio.file.Paths

import scala.io.Source

object Dictionary {
  val DictionaryPath = Paths.get(".")
    .resolve("src")
    .resolve("main")
    .resolve("resources")
    .resolve("google-10000-english.txt").toAbsolutePath.toString

  val Words = Source.fromFile(DictionaryPath).getLines().toIterable

}
