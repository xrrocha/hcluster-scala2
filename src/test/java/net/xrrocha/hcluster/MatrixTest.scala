package net.xrrocha.hcluster

import org.scalatest.FunSuite

class MatrixTest extends FunSuite {

  import Matrix._

  test("Computes dot product") {
    val vector1 = List(1d, 2d, 3d)
    val vector2 = List(2d, 4d, 6d)
    assert(28 == dotProd(vector1, vector2))
  }

  test("Transposes matrix") {
    val matrix = List(
      List(1.0, 2.0, 3.0),
      List(4.0, 5.0, 6.0)
    )
    assert(transpose(matrix) == List(
      List(1.0, 4.0),
      List(2.0, 5.0),
      List(3.0, 6.0),
    ))
  }

  test("Multiplies matrices") {
    val matrix1 = List(List(2.0, 0.0),
      List(3.0, -1.0),
      List(0.0, 1.0),
      List(1.0, 1.0))
    val matrix2 = List(List(1.0, 0.0, 2.0),
      List(4.0, -1.0, 0.0))
    assert(product(matrix1, matrix2) == List(
      List(2.0, 0.0, 4.0),
      List(-1.0, 1.0, 6.0),
      List(4.0, -1.0, 0.0),
      List(5.0, -1.0, 2.0)))
  }

  test("Product of matrix and transpose is symmetric") {
        val matrix1 = List(
          List(0.0, 0.0, 0.0, 0.24413606414846878, 0.0, 0.0, 0.09010335735736986, 0.0, 0.0, 0.04505167867868493),
          List(0.0, 0.0, 0.0, 0.0, 0.09010335735736986, 0.0, 0.09010335735736986, 0.0, 0.0, 0.04505167867868493),
          List(0.0, 0.08450863758985458, 0.0, 0.0, 0.0935688711018841, 0.16901727517970916, 0.0, 0.08450863758985458, 0.16901727517970916, 0.0),
        )
//    val matrix1 = List(
//      List(1.0, 2.0, 3.0),
//      List(4.0, 5.0, 6.0)
//    )
    val matrix2 = transpose(matrix1)
    matrix1.foreach(println)
    println()
    matrix2.foreach(println)
    println()
    product(matrix1, matrix2).foreach(println)
    println()
    normalize(product(matrix1, matrix2)).foreach(println)
//    assert(product(matrix1, matrix2).zipWithIndex.forall {
//      case (row, index) => {
//        row(index) == 1d
//      }
//    })
  }
}
