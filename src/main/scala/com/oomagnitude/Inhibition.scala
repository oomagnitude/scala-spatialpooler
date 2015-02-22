package com.oomagnitude

object Inhibition {
  val MaxOverlapOrdering = {
    val ordering: Ordering[Overlap] = Ordering.by(o => (o.permanentOverlap, o.weightedOverlap))
    ordering.reverse
  }
}

trait Inhibition {
  def compete(overlaps: Iterable[Overlap]): Iterable[Int]
}

class GlobalInhibition(maxWinners: Int) extends Inhibition {
  import com.oomagnitude.Inhibition._

  override def compete(overlaps: Iterable[Overlap]): Iterable[Int] = {
    overlaps.toList.sorted(MaxOverlapOrdering).take(maxWinners).map(_.dendriteIndex)
  }
}
