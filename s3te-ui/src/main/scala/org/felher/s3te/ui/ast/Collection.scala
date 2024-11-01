package org.felher.s3te.ui.ast

import org.felher.beminar.*
import com.raquo.laminar.api.L.*
import org.felher.s3te
import org.felher.s3te.ui.Context

object Collection:
  val bem = Bem("/collection")

  def render(collection: Signal[s3te.Ast.Collection], context: Context): HtmlElement =
    val betterName = collection.map: col =>
      if col.name == "Option" && col.children.isEmpty then "None"
      else if col.name == "Option" then "Some"
      else if col.name == "List" && col.children.isEmpty then "Nil"
      else col.name

    val hideParens = collection.map: col =>
      List("Option", "List").contains(col.name) && col.children.isEmpty

    val highlight = context.state
      .map(_.highlight.map(_.path))
      .distinct
      .combineWith(collection.map(_.path).distinct)
      .map: (highlight, path) =>
        highlight.exists(highlighted => path.isChildOf(highlighted))

    val selfIsCollapsed = Var(false)
    val isCollapsed     = context.parentIsCollapsed.combineWith(selfIsCollapsed.signal).map(_ || _)

    val childContext = context.withCollapse(isCollapsed)

    div(
      bem(~highlight, ~isCollapsed),
      collection.distinctBy(_ => ()) --> (collection => selfIsCollapsed.set(guessSelfCollapsed(collection))),
      Expander
        .render(selfIsCollapsed.signal, Observer(_ => selfIsCollapsed.update(!_)))
        .amend(bem("/expander", "hide" -> context.parentIsCollapsed)),
      div(bem("/name"), child.text <-- betterName),
      div(bem("/opening-paren", ~hideParens), "("),
      div(bem("/indent", ~isCollapsed)),
      div(
        bem("/children", ~isCollapsed, "no-params" -> context.state.map(!_.showParams)),
        children <-- AstList.render(
          collection.map(_.children),
          childContext,
          info =>
            div(
              bem("/index", "isOnly" -> info.isOnly, "hidden" -> context.state.map(!_.showParams)),
              "[",
              info.index.toString,
              "] ="
            ),
          info => info.element.amend(bem("/element", "last" -> info.isLast))
        )
      ),
      div(
        bem("/closing-paren", ~hideParens),
        ")"
      )
    )

  private def guessSelfCollapsed(collection: s3te.Ast.Collection): Boolean =
    collection.size == collection.depth
