package net.xrrocha.hcluster

object Types {
  type Index = Int
  type Similarity = Double
  type Score = Double
  type SparseMatrix = Map[Int, Map[Int, Similarity]]
}
