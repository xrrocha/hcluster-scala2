package hcluster

import Types._

trait SimilarityMatrix {
  def size: Index
  protected[SimilarityMatrix] def map: Map[Int, Map[Int, Similarity]]

  def apply(i: Index, j: Index): Similarity = {
    validate(i); validate(j)

    if (i == j) 1d
    else {
      val (l, r) = SimilarityMatrix.orderedIndexes(i, j)
      map.getOrElse(l, Map.empty).getOrElse(r, 0d)
    }
  }

  def validate(index: Index) =
    if (!(index >= 0 && index < size))
      throw new IllegalArgumentException(s"Invalid index not between 0 and ${size - 1}:  $index")

  override def toString = s"$size: ${map.toString}"
}

object SimilarityMatrix {
  def apply(compare: (Index, Index) => Similarity, pairs: Seq[(Index, Index)], minThreshold: Similarity = 0d): SimilarityMatrix = {
    val triplets = pairs.par.map { case (i, j) =>
      (i, j, compare(i, j))
    }

    def addPair(accum: (Index, Map[Int, Map[Int, Similarity]]), triplet: (Index, Index, Similarity)): (Index, Map[Int, Map[Int, Similarity]]) = {
      val (maxIndex, map) = accum
      val (leftIndex, rightIndex, similarity) = triplet

      val pairMax = math.max(leftIndex, rightIndex)
      val newMaxIndex = if (pairMax > maxIndex) pairMax else maxIndex

      if (similarity <= minThreshold) (newMaxIndex, map)
      else {
        val (i, j) = orderedIndexes(leftIndex, rightIndex)
        (newMaxIndex, map + (i -> (map.getOrElse(i, Map.empty) + (j -> similarity))))
      }
    }

    val (maxIndex, similarityMap) = triplets.seq.foldLeft(0, Map[Int, Map[Int, Similarity]]())(addPair)

    new SimilarityMatrix {
      val map = similarityMap
      val size = maxIndex + 1
    }
  }

  def orderedIndexes(i: Index, j: Index) = {
    val l = math.min(i, j)
    val r = math.max(i, j)
    (l, r)
  }
}
