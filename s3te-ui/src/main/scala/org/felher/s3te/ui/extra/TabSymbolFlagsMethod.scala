package org.felher.s3te.ui.extra

import org.felher.s3te.ui
import com.raquo.laminar.api.L.*
import org.felher.s3te
import org.felher.s3te.Ast
import org.felher.beminar.Bem

object TabSymbolFlagsMethod:
  val bem = Bem("/symbol-flags-method")

  def render(ast: s3te.Ast): Option[(String, HtmlElement)] =
    ast match
      case _: Ast.Collection => None
      case _: Ast.Leaf       => None
      case _: Ast.Reference  => None
      case n: Ast.Node       =>
        n.methodData.get(s3te.MethodKey.DotSymbolDotFlags) match
          case Some(flags) =>
            Some(
              ".symbol.flags",
              div(
                bem("/table"),
                div(bem("/header-cell"), "Flag"),
                div(bem("/header-cell"), "Docs"),
                flags.map: flag =>
                  List(
                    td(bem("/flag"), flag.toString),
                    td(bem("/docs"), flag.getScaladocs)
                  )
              )
            )
          case _           => None
