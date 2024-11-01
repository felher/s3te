package org.felher.s3te.ui.ast

import org.felher.s3te.ui.state as S
import org.felher.beminar.Bem
import com.raquo.laminar.api.L.*
import org.felher.s3te
import org.felher.s3te.ui.Context

object Reference:
  val bem = Bem("/reference")

  def render(reference: Signal[s3te.Ast.Reference], context: Context): HtmlElement =
    div(
      bem,
      "Endless Recursion. Hover to highlight",
      onMouseEnter.compose(_.sample(reference)) --> (reference =>
        context.onStateEvent.onNext(S.Event.HighlightEvent("enter", reference.target, "ast"))
      ),
      onMouseLeave.compose(_.sample(reference)) --> (reference =>
        context.onStateEvent.onNext(S.Event.HighlightEvent("leave", reference.target, "ast"))
      )
    )
