package hcluster

import hcluster.Types._

case class Cluster(centroid: Index, intraSimilarity: Similarity, elements: Seq[Int]) {
  def similarity(other: Cluster, similarityMatrix: SimilarityMatrix): Similarity =
    Cluster.similarity(this, other, similarityMatrix)

  def toString[A](as: IndexedSeq[A]) =
    s"centroid: ${as(centroid)}, intraSimilarity: $intraSimilarity, elements: ${(elements map as) mkString ","}"
}

object Cluster {
  def apply(index: Index): Cluster = new Cluster(index, 0d, Vector(index))

  def apply(dendrogram: Dendrogram, similarityMatrix: SimilarityMatrix): Cluster = {
    val elements: IndexedSeq[Index] = dendrogram.clusters flatMap (_.elements)
    val (centroidIndex: Index, centroidIntraSimilarity: Similarity) = findCentroid(elements, similarityMatrix)
    new Cluster(centroidIndex, centroidIntraSimilarity, elements)
  }

  def findCentroid(elements: Seq[Int], similarityMatrix: SimilarityMatrix): (Index, Similarity) = {

    val leftSimilarityTriplets: IndexedSeq[(Index, Index, Similarity)] =
      for (i <- elements.indices; j <- i + 1 until elements.length)
        yield (elements(i), elements(j), similarityMatrix(elements(i), elements(j)))

    val rightSimilarityTriplets: IndexedSeq[(Index, Index, Similarity)] =
      leftSimilarityTriplets.map { case (leftElement, rightElement, similarity) =>
        (rightElement, leftElement, similarity)
      }

    val similarityPairs: Seq[(Index, Similarity)] =
      (leftSimilarityTriplets ++ rightSimilarityTriplets).
        groupBy { case (left: Index, _, _) => left }.
        map { case (element: Index, triplets: IndexedSeq[(Index, Index, Similarity)]) =>
          (element, triplets.map(_._3).sum / triplets.length)
        }.
        toSeq

    if (similarityPairs.isEmpty) (elements.head, 0d)
    else {
      val maxSimilarity = similarityPairs.map(_._2).max
      val centroid = similarityPairs.find(_._2 == maxSimilarity).get._1

      (centroid, maxSimilarity)
    }
  }

  def similarity(c1: Cluster, c2: Cluster, similarityMatrix: SimilarityMatrix): Similarity = {
    similarityMatrix(c1.centroid, c2.centroid)
    //    val similarities =
    //      for (i <- 0 until c1.elements.length; j <- 0 until c2.elements.length)
    //      yield similarityMatrix(i, j)
    //    if (similarities.length == 0) 0d
    //    else similarities.sum / similarities.length
  }
}

