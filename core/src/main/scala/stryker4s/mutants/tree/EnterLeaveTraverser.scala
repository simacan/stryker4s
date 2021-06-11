package stryker4s.mutants.tree

import cats.data.{Chain, Writer}
import mutationtesting.Location
import stryker4s.extension.TreeExtensions.IsEqualExtension
import stryker4s.extension.mutationtype._
import stryker4s.model.Mutable
import scala.meta.inputs.Position
import scala.meta.transversers.SimpleTraverser
import scala.meta.{Defn, Term, Tree}

class EnterLeaveTraverser(onEnter: Tree => Unit, onLeave: Tree => Unit) extends SimpleTraverser {
  override def apply(tree: Tree): Unit = {
    onEnter(tree)
    super.apply(tree)
    onLeave(tree)
  }
}
