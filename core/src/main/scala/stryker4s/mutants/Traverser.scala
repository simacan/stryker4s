package stryker4s.mutants

import cats.data.{Chain, Writer}
import mutationtesting.Location
import stryker4s.extension.TreeExtensions.IsEqualExtension
import stryker4s.extension.mutationtype._
import stryker4s.model.Mutable

import scala.meta.inputs.Position
import scala.meta.transversers.SimpleTraverser
import scala.meta.{Defn, Term, Tree}
import stryker4s.mutants.tree.EnterLeaveTraverser

trait Traverser {
  type LastTopStatement = Tree
  type TreeWriter = Writer[Chain[Mutable], LastTopStatement]

  protected def canPlace(currentTree: Tree, lastTopStatement: LastTopStatement): Boolean

  protected def findMutants: PartialFunction[(Tree, LastTopStatement), Chain[Mutable]]

  def run(
      tree: Tree
  ) = {
    var writer: TreeWriter = Writer(Chain.empty, tree)
    val toMutantImpl = findMutants.lift

    val onEnter = (tree: Tree) => {
      val withNewTopStatement =
        writer.map(lastTopStatement => if (canPlace(tree, lastTopStatement)) tree else lastTopStatement)

      val lastTopStatement = withNewTopStatement.value

      writer = withNewTopStatement.tell(toMutantImpl((tree, lastTopStatement)).getOrElse(Chain.empty))
    }
    val onLeave = (tree: Tree) => ()

    new EnterLeaveTraverser(onEnter, onLeave)(tree)

    writer.run._1
  }

  protected def createMutants(
      original: Term,
      topStatement: Tree,
      location: Location,
      replacements: Term*
  ): Chain[Mutable] = {
    Chain.fromSeq(replacements.map { replacement =>
      val mutatedTopStatement = topStatement.transform { case t if t.isEqual(original) => replacement }
      Mutable(mutatedTopStatement, original.syntax, replacement.syntax, location)
    })
  }

  def toLocation(pos: Position): Location = Location(
    start = mutationtesting.Position(line = pos.startLine + 1, column = pos.startColumn + 1),
    end = mutationtesting.Position(line = pos.endLine + 1, column = pos.endColumn + 1)
  )
}

object Traverser {
  val mutantTraverser = new Traverser {
    def canPlace(currentTree: Tree, lastTopStatement: LastTopStatement): Boolean = {
      currentTree.parent.exists {
        case _: Term.Assign                              => true
        case _: Defn                                     => true
        case p if p.parent.exists(_ == lastTopStatement) => false
        case _: Term.Block                               => true
        case _: Term.If                                  => true
        case _: Term.ForYield                            => true
        case _                                           => false
      }
    }

    def findMutants: PartialFunction[(Tree, LastTopStatement), Chain[Mutable]] = {
      case (EqualTo(orig), topStatement) =>
        createMutants(orig, topStatement, toLocation(orig.pos), NotEqualTo.tree)
      case (NonEmptyString(orig), topStatement) =>
        createMutants(orig, topStatement, toLocation(orig.pos), EmptyString.tree)
    }
  }
}
