package com.oomagnitude

import java.nio.file.Paths

import scala.io.Source

object Dictionary {
  val DictionaryPath = Paths.get(".")
    .resolve("src")
    .resolve("main")
    .resolve("resources")
    .resolve("google-10000-english.txt").toAbsolutePath.toString

  // Take the first 1,000 most common English words
  val Words = Source.fromFile(DictionaryPath).getLines().take(1000).toIterable

  // For each character, the set of all words the character appears in
  val ReverseIndex = Words
    .flatMap(w => w.toLowerCase.toCharArray.toIterable.map(c => (c, w)))
    .groupBy(_._1)
    .map(kv => kv._1 -> kv._2.map(_._2).toSet)

}
