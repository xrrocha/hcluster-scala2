package hcluster

import com.typesafe.scalalogging.LazyLogging
import org.apache.lucene.search.spell.JaroWinklerDistance
import org.scalatest.FunSuite

import scala.collection.immutable

class SimilarityMatrixTest extends FunSuite with LazyLogging {

  val names = Vector(
    "alejandro", "alejito", "alejo",
    "malrene", "marlen", "marlene", "marleny",
    "marta", "martha",
    "ricardo")

  val pairs: immutable.IndexedSeq[(Int, Int)] = for {
    i <- names.indices
    j <- i + 1 until names.length
  } yield (i, j)

  val minSimilarity = 0d
  val jaroWinkler = new JaroWinklerDistance
  val compare: (Int, Int) => Double =
    (i: Int, j: Int) => jaroWinkler.getDistance(names(i), names(j)).toDouble

  val similarityMatrix = SimilarityMatrix(compare, pairs, minSimilarity)
  // logger.debug(s"Build similarity matrix: $similarityMatrix")

  test("Determines size correctly") {
    assert(similarityMatrix.size == names.length)
  }

  test("Accepts all valid indexes correctly") {
    for (i <- names.indices; j <- names.indices) {
      val score = compare(i, j)
      assert(similarityMatrix(i, j) == score)
      assert(similarityMatrix(j, i) == score)
    }
    assert(similarityMatrix(0, 9) > 0)
  }

  test("Rejects invalid indexes") {
    intercept[IllegalArgumentException] {
      similarityMatrix(-1, 0)
    }
    intercept[IllegalArgumentException] {
      similarityMatrix(0, -1)
    }
    intercept[IllegalArgumentException] {
      similarityMatrix(names.length, 0)
    }
    intercept[IllegalArgumentException] {
      similarityMatrix(0, names.length)
    }
  }

  test("Honors minimum similarity") {
    val minSimilarity = 0.7
    val similarityMatrix = SimilarityMatrix(compare, pairs, minSimilarity)
    assert(similarityMatrix(0, 9) == 0)
  }
}
