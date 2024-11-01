package org.felher.s3te.ui.ast

import org.felher.beminar.Bem
import com.raquo.laminar.api.L.*
import org.felher.s3te
import org.felher.s3te.ui.Context

object Ast:
  val bem = Bem("/ast")

  def render(ast: Signal[s3te.Ast], context: Context): Signal[HtmlElement] =
    ast.splitOne(_.ordinal): (_, init, sig) =>
      init match
        case _: s3te.Ast.Node       => Node.render(sig.asInstanceOf, context)
        case _: s3te.Ast.Leaf       => Leaf.render(sig.asInstanceOf, context)
        case _: s3te.Ast.Collection => Collection.render(sig.asInstanceOf, context)
        case _: s3te.Ast.Reference  => Reference.render(sig.asInstanceOf, context)
