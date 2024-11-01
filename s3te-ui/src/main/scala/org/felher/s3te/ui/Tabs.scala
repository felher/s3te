package org.felher.s3te.ui

import org.felher.beminar.*
import com.raquo.laminar.api.L.*

object Tabs:
  val bem = Bem("/tabs")
  val bemSubgrid = bem("subgrid")

  def render(tabs: Signal[List[(String, HtmlElement)]]): HtmlElement =
    val content = Var(Option.empty[HtmlElement])
    div(
      bem,
      tabs --> (tabs =>
        if content.now().isEmpty then
          tabs.headOption.foreach: (_, tabContent) =>
            content.set(Some(tabContent))
      ),
      div(
        bem("/beans"),
        children <-- tabs.map: tabs =>
          tabs.map: (name, tabContent) =>
            div(
              bem("/bean", "active" -> content.signal.map(_.exists(_ eq tabContent))),
              onClick --> (_ => content.set(Some(tabContent))),
              name
            )
      ),
      div(
        bem("/content"),
        children <-- content.signal.map(_.toList)
      )
    )
