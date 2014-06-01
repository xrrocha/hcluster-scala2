package hcluster

import Types._

trait PairGenerator {
  def pairsFrom[A](elements: IndexedSeq[A]): IndexedSeq[(Index, Index)]
}

// TODO Add unit tests for ExahaustivePairGenerator
trait ExahaustivePairGenerator extends PairGenerator {
  def pairsFrom[A](elements: IndexedSeq[A]) = ExahaustivePairGenerator.pairsFrom(elements.length)
}
object ExahaustivePairGenerator {
  def pairsFrom(length: Int): IndexedSeq[(Index, Index)] =
    for (i <- 0 until length; j <- i + 1 until length)
      yield (i, j)
}