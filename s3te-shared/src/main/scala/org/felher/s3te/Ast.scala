package org.felher.s3te

enum Ast derives JsonEncoder:
  case Node(
      path: Path,
      tpe: ReflectionType,
      span: Option[Span],
      children: List[Ast],
      methodData: MethodMap
  )
  case Collection(path: Path, name: String, children: List[Ast])
  case Reference(path: Path, target: Path)
  case Leaf(path: Path, raw: String)

  def path: Path

  lazy val depth: Int = this match
    case n: Node       => 1 + n.children.maxByOption(_.depth).fold(0)(_.depth)
    case c: Collection => 1 + c.children.maxByOption(_.depth).fold(0)(_.depth)
    case _: Leaf       => 1
    case _: Reference  => 1

  lazy val size: Int = this match
    case n: Node       => 1 + n.children.map(_.size).sum
    case c: Collection => 1 + c.children.map(_.size).sum
    case _: Leaf       => 1
    case _: Reference  => 1

  def withSpan(span: Span): Ast = this match
    case Node(path, tpe, _, params, methodData) => Node(path, tpe, Some(span), params, methodData)
    case Collection(path, name, elems)          => Collection(path, name, elems)
    case Leaf(path, value)                      => Leaf(path, value)
    case r: Reference                           => r

  def withMethodData[V](method: MethodKey[V], data: V): Ast = this match
    case Node(path, tpe, span, params, methodData) =>
      Node(path, tpe, span, params, methodData.add(method, data))
    case Collection(path, name, elems)             => Collection(path, name, elems)
    case Leaf(path, value)                         => Leaf(path, value)
    case Reference(path, referee)                  => Reference(path, referee)
