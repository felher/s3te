package org.felher.s3te

enum MethodKey[DataType] derives CanEqual:
  case DotTpe            extends MethodKey[Ast]
  case DotSymbol         extends MethodKey[Ast]
  case DotSymbolDotFlags extends MethodKey[List[Flag]]

  def methodSyntaxName: String = this match
    case DotTpe            => ".tpe"
    case DotSymbol         => ".symbol"
    case DotSymbolDotFlags => ".symbol.flags"

  def asAstKey: Option[MethodKey[Ast]] = this match
    case DotTpe            => Some(DotTpe)
    case DotSymbol         => Some(DotSymbol)
    case DotSymbolDotFlags => None

object MethodKey:
  given CanEqual[MethodKey[?], MethodKey[?]] = CanEqual.derived

  given JsonEncoder[MethodKey[?]] =
    JsonEncoder.stringEncoder.contramap[MethodKey[?]](_.toString)

  def valueEncoder[V](key: MethodKey[V]): JsonEncoder[V] = key match
    case DotTpe            => summon[JsonEncoder[Ast]]
    case DotSymbol         => summon[JsonEncoder[Ast]]
    case DotSymbolDotFlags => summon[JsonEncoder[List[Flag]]]
