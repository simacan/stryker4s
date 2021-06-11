package stryker4s.mutants

import cats.data.Chain
import stryker4s.model.Mutable

import scala.meta.Tree

trait MutantPlacer {
  def placeMutants(mutants: Chain[(Int, Mutable)], originalStatement: Tree): Tree
}
