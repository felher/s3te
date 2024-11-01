package org.felher.s3te.ui.extra

import org.felher.s3te.ui
import com.raquo.laminar.api.L.*
import org.felher.s3te
import org.felher.s3te.ui.state as S

object TabSymbolMethod:
  def render(whole: s3te.Ast, initialDrill: s3te.Path): Option[(String, HtmlElement)] =
    S.State(
      code = "",
      whole = whole,
      spans = Map.empty,
      drill = s3te.Path.empty,
      showParams = true,
      showActions = true,
      typeColors = true,
      highlight = None
    ).handleEvent(S.Event.SetDrill(initialDrill)) match
      case Left(value)      => None
      case Right(initState) =>
        val state        = Var(initState)
        val onStateEvent = Observer: (e: org.felher.s3te.ui.state.Event) =>
          val newState = state.now().handleEvent(e)
          newState match
            case Left(msg)       => org.scalajs.dom.window.alert(msg)
            case Right(newState) => state.set(newState)

        Some(
          ".symbol",
          div(
            child <-- org.felher.s3te.ui.ast.Ast.render(
              state.signal.map(_.root),
              org.felher.s3te.ui.Context.start(
                ui.ScalaDocHover.instance,
                Extra.instance,
                state.signal,
                onStateEvent
              )
            )
          )
        )
