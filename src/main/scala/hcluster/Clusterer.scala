package hcluster

import Types._

// TODO Support minSimilarity = 0d
trait Clusterer[A] { this: PairGenerator with SimilarityMetric[A] with ClusterEvaluator =>
  def cluster(elements: IndexedSeq[A]): (Score, Seq[Seq[A]]) = {
    val pairs = pairsFrom(elements)
    def doCompare(i: Index, j: Index) = compare(elements(i), elements(j))

    val similarityMatrix = SimilarityMatrix(doCompare, pairs, lowThreshold)
    val seedClusters = (0 until elements.length).map(Cluster(_))
    val (evaluation, clusters) = agglomerate(seedClusters, (evaluate(seedClusters, similarityMatrix), seedClusters), similarityMatrix)

    (evaluation, clusters.map(_.elements map elements))
  }

  private def agglomerate(clusters: Seq[Cluster], bestSoFar: (Score, Seq[Cluster]), similarityMatrix: SimilarityMatrix): (Score, Seq[Cluster]) = {
    val (bestScoreSoFar, bestClustersSoFar) = bestSoFar

    val (newScore, newClusters) = cluster(clusters, similarityMatrix)

    if (newClusters.length == clusters.length)
      (bestScoreSoFar, bestClustersSoFar)
    else {
      val nextBestSoFar =
        if (isScoreBetter(bestScoreSoFar, newScore))
          (bestScoreSoFar, bestClustersSoFar)
        else
          (newScore, newClusters)

      agglomerate(newClusters, nextBestSoFar, similarityMatrix)
    }
  }

  private def cluster(clusters: Seq[Cluster], similarityMatrix: SimilarityMatrix): (Score, Seq[Cluster]) = {
    val dendrogram = Dendrogram(clusters, similarityMatrix)

    val thresholds = Dendrogram.thresholds(dendrogram) filter (_ > lowThreshold)

    if (thresholds isEmpty) (evaluate(clusters, similarityMatrix), clusters)
    else {
      val evaluations =  thresholds map { threshold =>
        val cuts = dendrogram cutAt threshold
        val clusters =  cuts map (Cluster(_, similarityMatrix))
        (evaluate(clusters, similarityMatrix), clusters)
      }

      val bestScore = selectBestScore(evaluations.map(_._1))

      val bestClusters = evaluations.find(_._1 == bestScore).get

      bestClusters
    }
  }
}
