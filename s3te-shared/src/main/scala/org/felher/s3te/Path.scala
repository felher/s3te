package org.felher.s3te

opaque type Path <: Matchable = List[PathEntry]

object Path:
  given CanEqual[Path, Path] = CanEqual.derived
  given Ordering[Path]       = scala.math.Ordering.Implicits.seqOrdering
  given JsonEncoder[Path]    = JsonEncoder.listEncoder[PathEntry]

  extension (path: Path)
    def /(index: Int): Path              = path :+ PathEntry.ExtractorChild(index)
    def /(method: MethodKey[?]): Path    = path :+ PathEntry.MethodCall(method)
    def /(other: Path): Path             = path ++ other
    def stringify: String                = path.mkString("[", "/", "]")
    def lookupIn(ast: Ast): LookupResult =
      path.foldLeft(LookupResult.FoundAst(ast)): (lr, index) =>
        lr match
          case LookupResult.NotFound              => LookupResult.NotFound
          case LookupResult.FoundMethodLeaf(_, _) => LookupResult.NotFound
          case LookupResult.FoundAst(ast)         =>
            index match
              case PathEntry.ExtractorChild(index) =>
                ast match
                  case n: Ast.Node       => LookupResult.fromAstOption(n.children.lift(index))
                  case c: Ast.Collection => LookupResult.fromAstOption(c.children.lift(index))
                  case _                 => LookupResult.NotFound

              case PathEntry.MethodCall(k) =>
                ast match
                  case n: Ast.Node =>
                    n.methodData.get(k) match
                      case None    => LookupResult.NotFound
                      case Some(v) => LookupResult.fromKeyAndValue(k, v)
                  case _           => LookupResult.NotFound

    def allInits: List[Path] =
      path.inits.toList.reverse

    def splitHead: Option[(PathEntry, Path)] =
      path match
        case Nil          => None
        case head :: tail => Some((head, tail))

    def isChildOf(other: Path): Boolean =
      path.startsWith(other)

    def dropRight(n: Int): Path =
      path.dropRight(n)

  val empty: Path = Nil
