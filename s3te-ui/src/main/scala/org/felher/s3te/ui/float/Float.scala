package org.felher.s3te.ui.float

import com.raquo.laminar.api.L.*
import org.felher.beminar.{Bem, unary_~}

class Float:
  import Float.*

  private val calculator  = Calculator()
  private val state       = Var(Option.empty[State])
  private def open        = state.signal.map(_.isDefined)
  private def showOverlay = open.combineWith(state.signal.map(_.exists(_.positioning.useOverlay))).map(_ && _)

  val mount = div(
    bem(~open),
    div(
      bem("/overlay", ~showOverlay),
      inContext(ctx => onClick.filterByTarget(_ eq ctx.ref) --> (_ => state.set(None)))
    ),
    div(
      bem("/content", ~open),
      child.maybe <-- state.signal
        .map(_.map(_.content))
        .map(_.map(_.amend(styleAttr <-- calculator.calcSignal.map(_.transform), bem("/content-content")))),
      styleAttr <-- calculator.calcSignal.map(_.outerStyle),
      div(
        bem("/arrow", "pos" -> calculator.calcSignal.map(_.arrowPos)),
        svg.svg(
          svg.viewBox("0 0 12 12"),
          svg.width("12"),
          svg.height("12"),
          svg.path(
            svg.d("M 0 0 L 6 6 L 12 0"),
            svg.fill("hsl(0 0% 90%)"),
            svg.stroke("hsl(0 0% 60%)"),
            svg.strokeWidth("1")
          )
        )
      ),
      inContext(ctx =>
        state.signal --> (state =>
          org.scalajs.dom.window.setTimeout(() => calculator.update(ctx, state), 0)
          ()
        )
      )
    )
  )

  def open(
      target: org.scalajs.dom.Element,
      content: HtmlElement,
      positioning: Positioning
  ): Unit =
    state.set(Some(State(content, target, positioning)))

  def close(target: org.scalajs.dom.Element): Unit =
    state.update(_.filterNot(_.target eq target))

object Float:
  val bem = Bem("/float")
