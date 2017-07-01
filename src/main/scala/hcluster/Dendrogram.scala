package hcluster

import com.typesafe.scalalogging.LazyLogging
import hcluster.Types._

trait Dendrogram {
  def clusters: IndexedSeq[Cluster] = collectClusters(Vector())

  def collectClusters(accum: IndexedSeq[Cluster]): IndexedSeq[Cluster]

  def cutAt(threshold: Similarity) = doCutAt(Vector(), threshold)

  def doCutAt(accum: IndexedSeq[Dendrogram], threshold: Similarity): IndexedSeq[Dendrogram]
}

case class Leaf(cluster: Cluster) extends Dendrogram {
  def collectClusters(accum: IndexedSeq[Cluster]) = cluster +: accum

  def doCutAt(accum: IndexedSeq[Dendrogram], threshold: Similarity) = this +: accum
}

case class Branch(similarity: Similarity, left: Dendrogram, right: Dendrogram) extends Dendrogram {
  def collectClusters(accum: IndexedSeq[Cluster]) = right.collectClusters(left.collectClusters(accum))

  def doCutAt(accum: IndexedSeq[Dendrogram], threshold: Similarity) =
    if (similarity >= threshold) this +: accum
    else right.doCutAt(left.doCutAt(accum, threshold), threshold)
}

object Dendrogram extends LazyLogging {

  def apply(size: Int, similarityMatrix: SimilarityMatrix): Dendrogram = {
    val seedPairs: IndexedSeq[(Index, Leaf)] = for (i <- 0 until size) yield (i, Leaf(Cluster(i)))
    val seedMap: Map[Index, Dendrogram] = Map[Int, Dendrogram](seedPairs: _*)
    val (index: Index, dendrograms: Map[Index, Dendrogram]) = agglomerate(size, seedMap, similarityMatrix)
    dendrograms(index - 1)
  }

  // TODO Add unit test to Dendrogram.apply(IndexedSeq[Cluster, SimilarityMatrix)
  def apply(clusters: Seq[Cluster], similarityMatrix: SimilarityMatrix): Dendrogram = {
    val seedPairs: IndexedSeq[(Index, Leaf)] = for (i <- clusters.indices) yield (i, Leaf(clusters(i)))
    val seedMap: Map[Index, Dendrogram] = Map[Int, Dendrogram](seedPairs: _*)
    val (index: Index, dendrograms: Map[Index, Dendrogram]) = agglomerate(clusters.length, seedMap, similarityMatrix)
    dendrograms(index - 1)
  }

  def thresholds(dendrogram: Dendrogram): Seq[Similarity] = {
    def go(accum: Seq[Similarity], dendrogram: Dendrogram): Seq[Similarity] = dendrogram match {
      case Leaf(_) => accum
      case Branch(similarity, left, right) => similarity +: go(go(accum, left), right)
    }

    go(Seq(), dendrogram).distinct.sorted
  }

  def toDot[A](dendrogram: Dendrogram, elements: Seq[A], label: String = "Dendrogram"): String = {
    def collectNodes(accum: Seq[(String, String)], path: Seq[Int], dendrogram: Dendrogram): Seq[(String, String)] = {
      val nodeName = s"n_${path mkString "_"}"
      dendrogram match { // Stream
        case Leaf(Cluster(centroid, _, _)) =>
          (nodeName, elements(centroid).toString) +: accum
        case Branch(similarity, left, right) =>
          (nodeName, s"$similarity") +: collectNodes(collectNodes(accum, path ++ Seq(0), left), path ++ Seq(1), right)
      }
    }

    val nodePairs: Seq[(String, String)] = collectNodes(Seq(), Seq(0), dendrogram)

    def collectEdges(accum: Seq[(String, String)], path: Seq[Int], dendrogram: Dendrogram): Seq[(String, String)] = {
      val nodeName = s"n_${path mkString "_"}"
      dendrogram match { // Stream
        case Leaf(Cluster(centroid, _, _)) => accum
        case Branch(similarity, left, right) =>
          Seq((nodeName, s"${nodeName}_0"), (nodeName, s"${nodeName}_1")) ++
            collectEdges(collectEdges(accum, path ++ Seq(0), left), path ++ Seq(1), right)
      }
    }

    val edgePairs: Seq[(String, String)] = collectEdges(Seq(), Seq(0), dendrogram)

    val dot =
      s"""
         |digraph Dendrogram {
         |  label = "$label"
         |  graph [rankdir="LR"]
         |  node [shape="rectangle" style=filled color=blue fontcolor=white] ;
         |  ${(nodePairs map { case (nodeName, nodeLabel) => s"""$nodeName [label = "$nodeLabel"];""" }) mkString "\n  "}
         |  ${(edgePairs map { case (parent, child) => s"""$parent -> $child""" }) mkString "\n  "}
         |}
      """.stripMargin
    dot
  }

  private def agglomerate(mapIndex: Int, dendrograms: Map[Int, Dendrogram], similarityMatrix: SimilarityMatrix): (Index, Map[Int, Dendrogram]) =
    if (dendrograms.size == 1) (mapIndex, dendrograms)
    else {
      val (newmapIndex: Index, newDendrograms: Map[Index, Dendrogram]) = merge(mapIndex, dendrograms, similarityMatrix)
      agglomerate(newmapIndex, newDendrograms, similarityMatrix)
    }

  private def merge(mapIndex: Int, dendrograms: Map[Int, Dendrogram], similarityMatrix: SimilarityMatrix): (Index, Map[Int, Dendrogram]) = {
    val dendrogramVector: Vector[(Index, Dendrogram)] = dendrograms.toVector

    val pairs: IndexedSeq[(Index, Index)] = for {
      i <- dendrogramVector.indices
      j <- i + 1 until dendrogramVector.length
    } yield (i, j)

    def dendrogramSimilarity(leftDendrogram: Dendrogram, rightDendrogram: Dendrogram): Similarity = {
      val similarities: IndexedSeq[Similarity] = for {
        leftCluster <- leftDendrogram.clusters
        leftCentroid = leftCluster.centroid
        rightCluster <- rightDendrogram.clusters
        rightCentroid = rightCluster.centroid
      } yield similarityMatrix(leftCentroid, rightCentroid)

      if (similarities.isEmpty) 0d
      else similarities.sum / similarities.length
    }

    def similarityTriplet(pair: (Index, Index)): (Index, Index, Similarity) = {
      val (i: Index, j: Index) = pair
      val (leftIndex: Index, leftDendrogram: Dendrogram) = dendrogramVector(i)
      val (rightIndex: Index, rightDendrogram: Dendrogram) = dendrogramVector(j)
      (leftIndex, rightIndex, dendrogramSimilarity(leftDendrogram, rightDendrogram))
    }

    val similarityTriplets: IndexedSeq[(Index, Index, Similarity)] =
      (pairs map similarityTriplet) sortBy (-_._3) // 0 <= s <= 1

    def doMerge(indexMapPair: (Int, Map[Int, Dendrogram]), triplet: (Index, Index, Similarity)) = {
      val (i: Index, j: Index, similarity: Similarity) = triplet
      val (currentMapIndex: Index, dendrograms: Map[Index, Dendrogram]) = indexMapPair

      if (!((dendrograms contains i) && (dendrograms contains j)))
        indexMapPair
      else
        (currentMapIndex + 1, dendrograms - i - j + (currentMapIndex -> Branch(similarity, dendrograms(i), dendrograms(j))))
    }

    similarityTriplets.foldLeft(mapIndex, dendrograms)(doMerge)
  }
}
