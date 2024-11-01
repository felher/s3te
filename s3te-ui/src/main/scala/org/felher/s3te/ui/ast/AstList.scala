package org.felher.s3te.ui.ast

import com.raquo.laminar.api.L.*
import org.felher.s3te
import org.felher.s3te.ui.Context

object AstList:
  final case class AstListElement[A](
      element: A,
      index: Int,
      isLast: Signal[Boolean],
      key: Any
  ):
    val isOnly: Signal[Boolean] = isLast.map(isLast => isLast && index == 0)

  def render(
      nodes: Signal[List[s3te.Ast]],
      context: Context,
      forName: AstListElement[s3te.Ast] => HtmlElement,
      handleChild: AstListElement[HtmlElement] => HtmlElement
  ): Signal[List[HtmlElement]] =
    def addKeys(elements: List[s3te.Ast]): List[AstListElement[s3te.Ast]] =
      elements.zipWithIndex.map: (e, i) =>
        AstListElement(e, i, nodes.map(_.size == i + 1), (i, e.ordinal, e.path, i == elements.size - 1))

    nodes
      .map(addKeys)
      .split(_.key): (_, init, sig) =>
        val astSig = sig.map(_.element)

        val valueElement = init._1 match
          case _: s3te.Ast.Node       => Node.render(astSig.asInstanceOf, context)
          case _: s3te.Ast.Leaf       => Leaf.render(astSig.asInstanceOf, context)
          case _: s3te.Ast.Collection => Collection.render(astSig.asInstanceOf, context)
          case _: s3te.Ast.Reference  => Reference.render(astSig.asInstanceOf, context)

        val nameElement = forName(init)

        List(nameElement, handleChild(init.copy(element = valueElement)))
      .map(_.flatten)
