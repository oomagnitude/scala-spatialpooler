package com.oomagnitude

object Inhibition {
  /**
   * Sort overlaps first by raw number of overlapping connections, then by weighted overlaps
   */
  val MaxOverlapOrdering = {
    val ordering: Ordering[Overlap] = Ordering.by(o => (o.permanentOverlap, o.weightedOverlap))
    ordering.reverse
  }
}

/**
 * Construct for biased competition among a set of poolers with overlap to a particular input
 */
trait Inhibition {
  /**
   * Given a set of overlaps, determine which ones are the winners of the inhibition race
   *
   * @param overlaps the overlaps on competing poolers
   * @return the indexes of the winning poolers
   */
  def compete(overlaps: Iterable[Overlap]): Iterable[Int]
}

/**
 * Performs global inhibition by simply selecting the top N poolers as the winners
 *
 * @param maxWinners the max number of poolersthat may win the inhibition race
 */
class GlobalInhibition(maxWinners: Int) extends Inhibition {
  import com.oomagnitude.Inhibition._

  override def compete(overlaps: Iterable[Overlap]): Iterable[Int] = {
    overlaps.toList.sorted(MaxOverlapOrdering).take(maxWinners).map(_.poolerIndex)
  }
}
