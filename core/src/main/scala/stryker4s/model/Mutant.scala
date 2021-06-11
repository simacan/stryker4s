package stryker4s.model

import mutationtesting.Location
import stryker4s.extension.mutationtype.Mutation

import scala.meta.{Term, Tree}

final case class Mutant(id: Int, original: Term, mutated: Term, mutationType: Mutation[_ <: Tree])

final case class Mutable(mutatedTopStatement: Tree, original: String, replacement: String, location: Location)
