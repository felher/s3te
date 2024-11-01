package org.felher.s3te.ui

import org.felher.beminar.{Bem, unary_~}
import com.raquo.laminar.api.L.*
import org.felher.s3te
import org.felher.s3te.ui.state as S

object Code:
  val bem = Bem("/code")

  def render(
      partials: List[Spanner.Partial],
      state: Signal[S.State],
      onStateEvent: Observer[S.Event]
  ): HtmlElement =
    div(
      bem,
      partials.map: partial =>
        val highlight      = state.map(_.highlight.map(_.path)).map(_.exists(partial.paths.contains))
        val highlightStart = state.map(_.highlight.map(_.path)).map(_.exists(partial.starts.contains))
        val highlightEnd   = state.map(_.highlight.map(_.path)).map(_.exists(partial.ends.contains))
        val zeroWidth      = partial.span.empty
        val isEmpty        = partial.paths.isEmpty

        span(
          inContext(el =>
            state.changes
              .map(_.highlight)
              .collectSome
              .map(h => h.isClick && h.source == "ast" && partial.starts.contains(h.path))
              .distinct
              .filter(_ == true) --> (_ => {
              el.ref.asInstanceOf[scalajs.js.Dynamic].scrollIntoView(scalajs.js.Dynamic.literal(behavior = "smooth"))
              ()
            })
          ),
          bem("/partial", ~highlight, ~highlightStart, ~highlightEnd, ~isEmpty),
          mouseProp("enter", partial.paths, state, onStateEvent),
          mouseProp("leave", partial.paths, state, onStateEvent),
          mouseProp("click", partial.paths, state, onStateEvent),
          partial.text,
          div(bem("/border", "start", ~zeroWidth, "highlight" -> highlightStart)),
          div(bem("/border", "end", ~zeroWidth, "highlight" -> highlightEnd)),
        )
    )

  private def mouseProp(
      tpe: "enter" | "leave" | "click",
      paths: Set[s3te.Path],
      state: Signal[S.State],
      onStateEvent: Observer[S.Event]
  ): Modifier.Base =
    val prop = tpe match
      case "enter" => onMouseOver
      case "leave" => onMouseOut
      case "click" => onClick

    prop.compose(_.sample(state)) --> (s =>
      paths
        .filter(p => s.spans.contains(p))
        .minByOption(p => s.spans(p).length)
        .foreach: p =>
          onStateEvent.onNext(S.Event.HighlightEvent(tpe, p, "code"))
    )
