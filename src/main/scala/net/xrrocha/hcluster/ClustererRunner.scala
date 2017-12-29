package net.xrrocha.hcluster

import java.io.{File, FileWriter, PrintWriter}

import com.typesafe.scalalogging.StrictLogging
import net.xrrocha.hcluster.Types.Similarity
import org.apache.lucene.search.spell.LevensteinDistance

import scala.io.Source

object ClustererRunner extends App with StrictLogging {
  val start = .8
  val end = .95
  val step = 0.5
  val metrics = for (threshold <- start to end by step) yield {
    val clusterer = new Clusterer[String]
      with LuceneSimilarityMetric
      with ExahaustivePairGenerator
      with MaxIntraSimilarityClusterEvaluator // DaviesBouldinClusterEvaluator
    {
      val distance = new LevensteinDistance
      override val lowThreshold: Similarity = threshold
    }

    val names = Source.fromFile("data/spanish-surnames.tsv").getLines.take(4072).toVector

    val (pair, elapsedTime) = time(clusterer.cluster(names))
    val (score, clusters) = pair
    (threshold, clusters, score, elapsedTime)
  }

  metrics foreach { case (threshold, clusters, score, elapsedTime) =>
    logger.debug(s"$threshold\t${clusters.length}\t$score\t$elapsedTime")
  }

  val bestScore = metrics.map(_._3).max
  val (bestThreshold, bestClusters, _, bestElapsedTime) = metrics.find(_._3 == bestScore).get

  val clusterFile = new File("data/clusters.tsv")
  val out = new PrintWriter(new FileWriter(clusterFile), true)
  bestClusters.sortBy(-_.length) foreach { cluster =>
    val clusterLine = cluster.mkString(",")
    out.println(s"${cluster.length}\t$clusterLine")
    logger.debug(s"${cluster.length}: $clusterLine")
  }
  out.close()
  logger.debug(s"${bestClusters.length} clusters with score $bestScore found at threshold $bestThreshold in $bestElapsedTime milliseconds")

  def time[A](a: => A): (A, Long) = {
    val startTime = System.currentTimeMillis()
    val result = a
    val endTime = System.currentTimeMillis()
    (result, endTime - startTime)
  }
}
