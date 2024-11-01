package org.felher.s3te.ui.ast

import org.felher.beminar.Bem
import com.raquo.laminar.api.L.*
import org.felher.s3te
import org.felher.s3te.ui.Context

object Leaf:
  val bem = Bem("/leaf")

  def render(ast: Signal[s3te.Ast.Leaf], context: Context): HtmlElement =
    div(
      bem,
      text <-- ast.map(_.raw)
    )
