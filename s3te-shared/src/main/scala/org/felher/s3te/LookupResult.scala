package org.felher.s3te

enum LookupResult derives CanEqual:
  case NotFound
  case FoundAst(ast: Ast)
  case FoundMethodLeaf[V](key: MethodKey[V], value: V)(using scala.util.NotGiven[V <:< Ast])

  def asAst: Option[Ast] = this match
    case NotFound              => None
    case FoundAst(ast)         => Some(ast)
    case FoundMethodLeaf(_, _) => None

object LookupResult:
  def fromAstOption(ast: Option[Ast]): LookupResult = ast match
    case None    => NotFound
    case Some(a) => FoundAst(a)

  def fromKeyAndValue[V](key: MethodKey[V], value: V): LookupResult =
    key match
      case MethodKey.DotTpe            => FoundAst(value)
      case MethodKey.DotSymbol         => FoundAst(value)
      case MethodKey.DotSymbolDotFlags => FoundMethodLeaf(key, value)
