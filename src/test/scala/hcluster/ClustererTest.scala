package hcluster

import com.typesafe.scalalogging.LazyLogging
import hcluster.Types._
import org.apache.lucene.search.spell.JaroWinklerDistance
import org.scalatest.FunSuite

class ClustererTest extends FunSuite with LazyLogging {

  val names = IndexedSeq(
    "alejandro", "alejito", "alejo",
    "malrene", "marlen", "marlene", "marleny",
    "marta", "martha",
    "ricardo")

  test("Build proper clusters") {
    val clusterer = new Clusterer[String]
      with LuceneSimilarityMetric
      with ExahaustivePairGenerator
      with MaxIntraSimilarityClusterEvaluator {
      val distance = new JaroWinklerDistance
      override val lowThreshold: Similarity = 0.85d
    }

    val (score, clusters) = clusterer.cluster(names)
    assert(score == 0.7083333358168602)
    assert(clusters == Seq(
      Seq("ricardo"),
      Seq("martha", "marta"),
      Seq("malrene", "marleny", "marlen", "marlene"),
      Seq("alejandro", "alejo", "alejito")))
  }
}

object ClustererTest extends App {
  val start = .8
  val end = .95
  val step = 0.5
  val metrics = for (threshold <- start to end by step) yield {
    val clusterer = new Clusterer[String]
      with LuceneSimilarityMetric
      with ExahaustivePairGenerator
      with MaxIntraSimilarityClusterEvaluator // DaviesBouldinClusterEvaluator
    {
      val distance = new JaroWinklerDistance
      override val lowThreshold: Similarity = threshold
    }

    val names = io.Source.fromFile("data/spanish-surnames.tsv").getLines.take(500).toVector

    val (pair, elapsedTime) = time(clusterer.cluster(names))
    val (score, clusters) = pair
    (threshold, clusters, score, elapsedTime)
  }

  metrics foreach { case (threshold, clusters, score, elapsedTime) =>
    println(s"$threshold\t${clusters.length}\t$score\t$elapsedTime")
  }

  val bestScore = metrics.map(_._3).max
  val (bestThreshold, bestClusters, _, bestElapsedTime) = metrics.find(_._3 == bestScore).get

  bestClusters.sortBy(_.length) foreach { cluster =>
    println(s"${cluster.length}: ${cluster.mkString(",")}")
  }
  println(s"${bestClusters.length} clusters with score $bestScore found at threshold $bestThreshold in $bestElapsedTime milliseconds")

  def time[A](a: => A): (A, Long) = {
    val startTime = System.currentTimeMillis()
    val result = a
    val endTime = System.currentTimeMillis()
    (result, endTime - startTime)
  }
}
