package net.xrrocha.hcluster

import net.xrrocha.hcluster.ClustererRunner.ConsumerProfile
import net.xrrocha.hcluster.Types.Similarity
import org.scalatest.FunSuite

import scala.io.Source

class TfidfCalculationTest extends FunSuite {

  import Matrix._
  import TfIdfSimilarityMetric._

  test("phraseToTerms builds word list") {
    val document = "\t Pedro  habla\t\tde Juan "
    assert(phraseToTerms(document) == List("Pedro", "habla", "de", "Juan"))
  }

  test("Extracts terms from phrase") {
    val phrase = "Pedro habla de Juan y Juan habla de Pedro"
    val terms = phraseToTerms(phrase)
    assert(List("Juan", "Pedro", "de", "habla", "y") == distinctTerms(terms))
  }

  test("Extracts terms from documents") {
    val documents = List(
      "Pedro habla a Juan y Juan habla a Pedro",
      "Pedro habla de Juan y Juan habla de Pedro",
    )
      .map(phraseToTerms)
    val terms = distinctDocumentTerms(documents)
    assert(List("Juan", "Pedro", "a", "de", "habla", "y") == distinctTerms(terms))
  }

  test("Computes term frequencies") {
    val document = phraseToTerms("Pedro habla a Juan y Juan habla a Pedro")
    assert(termFrequencies(document) == Map(
      "Juan" -> 0.2222222222222222,
      "Pedro" -> 0.2222222222222222,
      "a" -> 0.2222222222222222,
      "habla" -> 0.2222222222222222,
      "y" -> 0.1111111111111111))
  }

  test("Computes inverse term frequency") {
    val documents = List(
      "Pedro habla a Juan y Juan habla a Pedro",
      "Pedro habla de Juan y Juan habla de Pedro",
    )
      .map(phraseToTerms)
    assert(inverseDocumentFrequency("Juan", documents) == 0.0)
    assert(inverseDocumentFrequency("Pedro", documents) == 0.0)
    assert(inverseDocumentFrequency("a", documents) == 0.6931471805599453)
    assert(inverseDocumentFrequency("de", documents) == 0.6931471805599453)
    assert(inverseDocumentFrequency("habla", documents) == 0.0)
    assert(inverseDocumentFrequency("y", documents) == 0.0)
  }

  test("Cluster categories") {
    val categories =
      Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("categories.txt"))
        .getLines()
        .toList
    //    val documents = List(
    //      "Pedro habla a Juan y Juan habla a Pedro",
    //      "Pedro habla de Juan y Juan habla de Pedro",
    //      "Lo que dice Pedro de Juan dice más de Pedro que de Juan"
    //    )

    val clusterer = new Clusterer[String]
      with TfIdfSimilarityMetric
      with ExahaustivePairGenerator
      with MaxIntraSimilarityClusterEvaluator {
      override def lowThreshold: Similarity = 0d
      override val documentList: List[String] = categories
    }
    val (score, clusters) = clusterer.cluster(categories.toIndexedSeq)
    println(s"Score: $score, clusters: ${clusters.size}")
    clusters
      .sortWith((c1, c2) => {
        c1.length > c2.length || {
          c1.length == c2.length && {
            val a1 = c1.map(_.split(",").length).sum
            val a2 = c2.map(_.split(",").length).sum
            a1 > a2
          }
        }
      })
      .foreach(cluster => println(s"${cluster.size}: ${cluster.mkString("   |   ")}"))
  }
}
