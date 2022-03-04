package net.xrrocha.hcluster

import com.typesafe.scalalogging.StrictLogging
import net.xrrocha.hcluster.Types.Similarity

import java.io._
import java.nio.charset.StandardCharsets
import scala.io.Source
import scala.math.{max, min}

object ClustererRunner extends App with StrictLogging {

  //noinspection SourceNotClosed
  val basename = "age-gender-city-categories"
  val lines = doWith[Source, List[String]](Source.fromFile(s"data/$basename.tsv"), { source =>
    source.getLines().toList
  })
  val elements = lines
    .map { line =>
      val Array(ageGroup, gender, city, categories) = line.split("\t")
      ConsumerProfile(ageGroup, gender, city, categories)
    }
    .toVector

  val tfidf = new TfIdfSimilarityMetric {
    override val documentList: List[String] = elements.map(_.categories).toList
  }

  case class ConsumerProfile(ageGroup: String, gender: String, city: String, categories: String) {
    private val age = ageGroup.toInt + 1

    def compareWith(that: ConsumerProfile): Double = (
      (min(this.age, that.age) / max(this.age, that.age)) +
        (if (this.gender == that.gender) 1 else 0) +
        (if (this.city == that.city) 1 else 0) +
        tfidf.compare(this.categories, that.categories)
      // (this.categories.intersect(that.categories).length.toDouble / this.categories.union(that.categories).length)
      ) / 4.0

    override def toString = s"$ageGroup\t$gender\t$city\t${categories}"
  }

  val clusterer = new Clusterer[ConsumerProfile]
    with SimilarityMetric[ConsumerProfile]
    with ExahaustivePairGenerator
    with MaxIntraSimilarityClusterEvaluator {
    override def lowThreshold: Similarity = 0.5d

    def compare(a1: ConsumerProfile, a2: ConsumerProfile): Similarity = a1.compareWith(a2)
  }

  val (pair, elapsedTime) = time(clusterer.cluster(elements))
  val (score, clusters) = pair

  val clusterFile = new File(s"data/$basename-clusters.tsv")
  val out = new PrintWriter(new OutputStreamWriter(new FileOutputStream(clusterFile), StandardCharsets.UTF_8), true)

  clusters.sortBy(-_.length).zipWithIndex.foreach { pair =>
    val (cluster, index) = pair
    cluster.foreach { element =>
      out.println(s"$index\t${cluster.size}\t$element")
    }
  }
  out.close()
  logger.debug(s"${clusters.length} clusters with score $score $elapsedTime milliseconds")

  def time[A](a: => A): (A, Long) = {
    val startTime = System.currentTimeMillis()
    val result = a
    val endTime = System.currentTimeMillis()
    (result, endTime - startTime)
  }

  def doWith[A <: Closeable, B](target: A, action: A => B): B =
    try {
      action(target)
    } finally {
      target.close()
    }
}
