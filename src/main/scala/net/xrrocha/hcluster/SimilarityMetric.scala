package net.xrrocha.hcluster

import net.xrrocha.hcluster.Matrix.{cosineSimilarity, normalize}
import net.xrrocha.hcluster.Types._

trait SimilarityMetric[A] {
  def lowThreshold: Similarity = 0d

  def compare(a1: A, a2: A): Similarity
}

trait LuceneSimilarityMetric extends SimilarityMetric[String] {

  import org.apache.lucene.search.spell.StringDistance

  def distance: StringDistance

  def compare(s1: String, s2: String): Similarity = distance.getDistance(s1, s2).toDouble
}

object Matrix {


  def dotProd(vector1: List[Double], vector2: List[Double]): Double =
    vector1.zip(vector2).map { pair => pair._1 * pair._2 }.sum

  def magnitude(vector: List[Double]): Double =
    math.sqrt(vector.map(x => x * x).sum)

  def cosineSimilarity(vector1: List[Double], vector2: List[Double]): Double =
    dotProd(vector1, vector2) / (magnitude(vector1) * magnitude(vector2))

  def cosineSimilarityMatrix(matrix: List[List[Double]]): List[List[Double]] = {
    val result = Array.ofDim[Double](matrix.size, matrix.size)
    for (i <- matrix.indices; j <- i until matrix.size) {
      val similarity =
        if (i == j) 1d
        else cosineSimilarity(matrix(i), matrix(j))
      result(i)(j) = similarity
      result(j)(i) = similarity
    }
    return result.map(_.toList).toList
  }

  def transpose(matrix: List[List[Double]]): List[List[Double]] =
    if (matrix.head.isEmpty) Nil
    else matrix.map(_.head) :: transpose(matrix.map(_.tail))

  def product(matrix1: List[List[Double]], matrix2: List[List[Double]]): List[List[Double]] = {
    val transposed = transpose(matrix2)
    for (row <- matrix1) yield
      for (column <- transposed) yield
        dotProd(row, column)
  }

  def normalizerFor(matrix: List[List[Double]]): Double => Double = {
    val values = matrix.flatten.distinct
    val maxValue = values.max
    val minValue = values.min
    val diffValue = maxValue - minValue
    value: Double => (value - minValue) / diffValue
  }

  def normalize(matrix: List[List[Double]]): List[List[Double]] = {
    val normalizer = normalizerFor(matrix)
    matrix.map(_.map(normalizer))
  }
}

object TfIdfSimilarityMetric {

  def phraseToTerms(phrase: String): List[String] = phrase.trim.split("\\s+").toList

  def distinctTerms(document: List[String]): List[String] =
    document.distinct.sorted

  def distinctDocumentTerms(documents: List[List[String]]): List[String] =
    documents.flatten.distinct.sorted

  def termFrequencies(document: List[String]): Map[String, Double] =
    document.groupBy(identity).mapValues(_.size / document.size.toDouble)

  def inverseDocumentFrequency(term: String, documents: List[List[String]]): Double = {
    math.log(documents.size / documents.count(_.contains(term)).toDouble)
  }
}

trait TfIdfSimilarityMetric extends SimilarityMetric[String] {

  def documentList: List[String]

  import TfIdfSimilarityMetric._

  override def compare(d1: String, d2: String): Similarity =
    similarityMatrix(documentToIndex(d1))(documentToIndex(d2))

  private lazy val documentToIndex = documentList.indices.map(i => (documentList(i), i)).toMap

  private lazy val similarityMatrix: List[List[Double]] = {

    val documents: List[List[String]] = documentList.map(phraseToTerms)

    val terms: List[String] = distinctDocumentTerms(documents)

    val termFrequenciesByDocument: List[Map[String, Double]] = documents.map(termFrequencies)

    val termInverseDocumentFrequencies: Map[String, Double] =
      terms.map(term => (term, inverseDocumentFrequency(term, documents))).toMap

    val documentByTermMatrix: List[List[Double]] = termFrequenciesByDocument.map { termFrequencies =>
      terms.map { term =>
        val termFrequency = termFrequencies.getOrElse(term, 0d)
        termFrequency * termInverseDocumentFrequencies(term)
      }
    }

    normalize(Matrix.cosineSimilarityMatrix(documentByTermMatrix))
  }
}
