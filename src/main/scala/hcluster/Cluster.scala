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
    val (centroid: Index, intraSimilarity: Similarity) = centroidPair(elements, similarityMatrix)
    new Cluster(centroid, intraSimilarity, elements)
  }

  def centroidPair(elements: Seq[Int], similarityMatrix: SimilarityMatrix): (Index, Similarity) = {
    val similarityTriplets: IndexedSeq[(Index, Index, Similarity)] =
      for (i <- elements.indices; j <- i + 1 until elements.length)
        yield (elements(i), elements(j), similarityMatrix(elements(i), elements(j)))

    val similarityPairs: Seq[(Index, Similarity)] =
      (similarityTriplets ++ similarityTriplets map (t => (t._2, t._1, t._3))).
        groupBy { case (left: Index, _, _) => left }.
        map { case (i: Index, t: IndexedSeq[(Index, Index, Similarity)]) => (i, t.map(_._3).sum / t.length) }.
        toSeq

    val orderedPairs: Seq[(Index, Similarity)] = similarityPairs sortBy (-_._2)

    orderedPairs.headOption getOrElse(elements.head, 0d)
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

