package com.oomagnitude

/**
 * Functionality for encoding an item from a hidden state (the outside world) into an SDR that can be processed by
 * the model
 *
 * @tparam T the type of item being encoded
 */
trait Encoder[T] {
  /**
   * @param item the item to encode
   * @return the encoding of the item, as an SDR consisting of all inputs that are turned on
   */
  def encode(item: T): Set[Int]
}

class WordEncoder(letterEncodings: Map[Char, Int]) extends Encoder[String] {

  /**
   * Encode a word as a set of inputs to the sensor. Takes each letter, maps it to an index for the sensor and
   * returns the result as a set.
   *
   * @param word the word to encode
   * @return the unique set of sensor indexes for the word
   */
  override def encode(word: String): Set[Int] = {
    val uniqueChars = word.toLowerCase.toCharArray.toSet
    uniqueChars.collect {case char if letterEncodings.contains(char) => letterEncodings(char)}
  }
}