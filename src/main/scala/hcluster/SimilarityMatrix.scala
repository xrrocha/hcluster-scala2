package hcluster

import hcluster.Types._

import scala.collection.parallel.ParSeq

trait SimilarityMatrix {
  def size: Index

  def apply(i: Index, j: Index): Similarity = {
    validate(i)
    validate(j)

    if (i == j) 1d
    else {
      val (l: Index, r: Index) = SimilarityMatrix.orderedIndexes(i, j)
      map.getOrElse(l, Map.empty).getOrElse(r, 0d)
    }
  }

  def validate(index: Index): Unit =
    if (!(index >= 0 && index < size))
      throw new IllegalArgumentException(s"Invalid index not between 0 and ${size - 1}:  $index")

  override def toString = s"$size: ${map.toString}"

  protected[SimilarityMatrix] def map: SparseMatrix
}

object SimilarityMatrix {
  def apply(compare: (Index, Index) => Similarity,
            pairs: Seq[(Index, Index)],
            minThreshold: Similarity = 0d): SimilarityMatrix = {
    val triplets: ParSeq[(Index, Index, Similarity)] =
      pairs.par.map { case (i, j) =>
        (i, j, compare(i, j))
      }

    def addPair(accum: (Index, SparseMatrix), triplet: (Index, Index, Similarity)): (Index, SparseMatrix) = {
      val (maxIndex: Index, map: SparseMatrix) = accum
      val (leftIndex: Index, rightIndex: Index, similarity: Similarity) = triplet

      val pairMax = math.max(leftIndex, rightIndex)
      val newMaxIndex = if (pairMax > maxIndex) pairMax else maxIndex

      if (similarity <= minThreshold) (newMaxIndex, map)
      else {
        val (i: Index, j: Index) = orderedIndexes(leftIndex, rightIndex)
        (newMaxIndex, map + (i -> (map.getOrElse(i, Map.empty) + (j -> similarity))))
      }
    }

    val (maxIndex: Index, similarityMap: SparseMatrix) =
      triplets.seq.foldLeft(0, Map[Int, Map[Int, Similarity]]())(addPair)

    new SimilarityMatrix {
      val map: SparseMatrix = similarityMap
      val size: Index = maxIndex + 1
    }
  }

  def orderedIndexes(i: Index, j: Index): (Index, Index) = {
    val l: Index = math.min(i, j)
    val r: Index = math.max(i, j)
    (l, r)
  }
}
