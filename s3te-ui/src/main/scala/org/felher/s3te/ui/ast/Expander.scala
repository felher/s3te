package org.felher.s3te.ui.ast

import org.felher.beminar.*
import com.raquo.laminar.api.L.*

object Expander:
  val bem = Bem("/expander")

  def render(collapsed: Signal[Boolean], onToggle: Observer[Unit]): HtmlElement =
    div(
      bem,
      onClick.mapToUnit --> onToggle,
      child <-- collapsed.signal.map:
        case false =>
          svg.svg(
            bem("/svg"),
            svg.viewBox := "0 0 448 512",
            svg.height  := "1em",
            svg.width   := "1em",
            svg.xmlns   := "http://www.w3.org/2000/svg",
            svg.path(
              bem("/path", ~collapsed),
              svg.transform := "translate(0, 50)",
              svg.d         := "M201.4 342.6c12.5 12.5 32.8 12.5 45.3 0l160-160c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L224 274.7 86.6 137.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l160 160z"
            )
          )

        case true =>
          svg.svg(
            bem("/svg"),
            svg.viewBox := "0 0 320 512",
            svg.height  := "1em",
            svg.width   := "1em",
            svg.xmlns   := "http://www.w3.org/2000/svg",
            svg.path(
              bem("/path", ~collapsed),
              svg.transform := "translate(0, 20)",
              svg.d         := "M278.6 233.4c12.5 12.5 12.5 32.8 0 45.3l-160 160c-12.5 12.5-32.8 12.5-45.3 0s-12.5-32.8 0-45.3L210.7 256 73.4 118.6c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0l160 160z"
            )
          )
    )
