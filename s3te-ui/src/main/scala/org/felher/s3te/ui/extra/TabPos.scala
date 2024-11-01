package org.felher.s3te.ui.extra

import org.felher.s3te.ui
import com.raquo.laminar.api.L.*
import org.felher.s3te
import org.felher.s3te.Ast

object TabPos:
  def render(ast: s3te.Ast): Option[(String, HtmlElement)] =
    ast match
      case _: Ast.Collection => None
      case _: Ast.Leaf       => None
      case _: Ast.Reference       => None
      case n: Ast.Node       =>
        n.span.map(span =>
          (".pos",
          div(
            span.toString
          )
        )
        )
