package com.oomagnitude.layer

import com.oomagnitude.dendrite.Overlap
import com.oomagnitude.geometry.Coordinate

object Inhibition {
  val MaxOverlapOrdering = {
    val ordering: Ordering[Overlap] = Ordering.by(o => (o.permanentOverlap, o.rawOverlap))
    ordering.reverse
  }
}

trait Inhibition {
  def compete(overlaps: Iterable[Overlap]): Iterable[Coordinate]
}

class GlobalInhibition(maxWinners: Int) extends Inhibition {
  import Inhibition._

  override def compete(overlaps: Iterable[Overlap]): Iterable[Coordinate] = {
    overlaps.toList.sorted(MaxOverlapOrdering).take(maxWinners).map(_.address)
  }
}
