package hcluster

import com.typesafe.scalalogging.LazyLogging
import hcluster.Types._
import org.apache.lucene.search.spell.LevensteinDistance
import org.scalatest.FunSuite

class ClusterTest extends FunSuite with LazyLogging {

  test("Builds cluster from branch dendrogram") {
    val names = Seq("malrene", "marleny", "marlen", "marlene")

    val pairs = for {
      i <- names.indices
      j <- i + 1 until names.length
    } yield (i, j)

    val minSimilarity = 0d
    val stringDistance = new LevensteinDistance
    val compare = (i: Int, j: Int) => stringDistance.getDistance(names(i), names(j)).toDouble

    val similarityMatrix = SimilarityMatrix(compare, pairs, minSimilarity)

    val dendrogram =
      Branch(
        similarity = 0.9555555582046509,
        left = Branch(
          similarity = 0.879365086555481,
          left = Leaf(Cluster(0)),
          right = Leaf(Cluster(1))
        ),
        right = Branch(
          similarity = 0.9809523820877075,
          left = Leaf(Cluster(2)),
          right = Leaf(Cluster(3))
        )
      )

    val cluster = Cluster(dendrogram, similarityMatrix)

    assert(cluster.centroid == 3)
    assert(cluster.intraSimilarity == 0.8095238208770752)
    assert(cluster.elements == Seq(3, 2, 1, 0))
  }

  test("Builds cluster from leaf dendrogram") {
    val similarityMatrix = new SimilarityMatrix {
      val size = 0
      val map: SparseMatrix = Map[Int, Map[Int, Similarity]]()
    }
    val cluster = Cluster(Leaf(Cluster(0)), similarityMatrix)
    assert(cluster.centroid == 0)
    assert(cluster.intraSimilarity == 0d)
    assert(cluster.elements == Seq(0))
  }

  test("Computes similarity with another cluster") {
    val names = Seq("alejo", "alejito", "malrene", "marleny", "marlen", "marlene")

    val pairs = for {
      i <- names.indices
      j <- i + 1 until names.length
    } yield (i, j)

    val minSimilarity = 0d
    val stringDistance = new LevensteinDistance
    val compare = (i: Int, j: Int) => stringDistance.getDistance(names(i), names(j)).toDouble

    val similarityMatrix = SimilarityMatrix(compare, pairs, minSimilarity)

    val leftCluster = Cluster(0, 0.9428572058677673, Seq(1, 0))
    val rightCluster = Cluster(2, 0.9809523820877075, Seq(3, 2, 1, 0))

    assert(leftCluster.similarity(rightCluster, similarityMatrix) == 0.4285714030265808)
    assert(rightCluster.similarity(leftCluster, similarityMatrix) == 0.4285714030265808)
  }
}
