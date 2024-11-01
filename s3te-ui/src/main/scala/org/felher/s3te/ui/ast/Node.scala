package org.felher.s3te.ui.ast

import org.felher.beminar.*
import com.raquo.laminar.api.L.*
import org.felher.s3te
import org.felher.s3te.ui.state as S
import org.felher.s3te.ui.Context

object Node:
  val bem = Bem("/node")

  def render(node: Signal[s3te.Ast.Node], context: Context): HtmlElement =
    def nameSlot(i: Int): HtmlElement =
      div(
        bem("/param-name", "hidden" -> context.state.map(!_.showParams)),
        child.text <-- node.map: node =>
          s3te.ReflectionType
            .extractorNames(node.tpe)
            .flatMap(names => names.lift(i))
            .getOrElse("?"),
        " = "
      )

    val highlight = context.state
      .map(_.highlight.map(_.path))
      .distinct
      .combineWith(node.map(_.path).distinct)
      .map: (highlight, path) =>
        highlight.exists(highlighted => path.isChildOf(highlighted))

    val selfIsCollapsed = Var(false)
    val isCollapsed     = context.parentIsCollapsed.combineWith(selfIsCollapsed.signal).map(_ || _)

    val childContext = context.withHighlight(highlight).withCollapse(isCollapsed)
    val rootType     = node.map(n => Node.getRootType(n.tpe))
    val noColors     = context.state.map(!_.typeColors)

    div(
      bem(~highlight, ~isCollapsed),
      node.distinctBy(_ => ()) --> (node => selfIsCollapsed.set(guessSelfCollapsed(node))),
      Expander
        .render(selfIsCollapsed.signal, Observer(_ => selfIsCollapsed.update(!_)))
        .amend(bem("/expander", "hide" -> context.parentIsCollapsed)),
      div(
        bem("/name", ~rootType, ~noColors),
        child.text <-- node.map(_.tpe.toString),
        inContext(el =>
          List(
            highlighterModifiers(el.ref, node, context),
            onMouseOver.compose(_.sample(node)) --> (node => context.hover.open(el.ref, node.tpe)),
            onMouseOut --> (_ => context.hover.close(el.ref))
          )
        )
      ),
      div(
        bem("/actions", "hidden" -> context.state.map(!_.showActions)),
        div(
          bem("/action-drill"),
          onClick.compose(_.sample(node)) --> (node => context.onStateEvent.onNext(S.Event.SetDrill(node.path)))
        ),
        div(
          bem("/action-info"),
          onClick.compose(_.sample(node, context.state)) --> ((node, state) =>
            context.extra.open(state.whole, node.path)
          )
        )
      ),
      div(bem("/opening-paren"), "("),
      div(bem("/indent", ~isCollapsed)),
      div(
        bem("/children", ~isCollapsed, "no-params" -> context.state.map(!_.showParams)),
        children <-- AstList.render(
          node.map(_.children),
          childContext,
          info => nameSlot(info.index),
          info => info.element
        )
      ),
      div(
        bem("/closing-paren"),
        ")"
      )
    )

  private def highlighterModifiers(
      el: org.scalajs.dom.Element,
      node: Signal[s3te.Ast.Node],
      c: Context
  ): Modifier.Base =
    List(
      c.state
        .combineWith(node)
        .map((s, n) => s.highlight.exists(h => h.isClick && h.source == "code" && h.path == n.path))
        .distinct
        .changes
        .filter(identity) --> (_ => {
        el.asInstanceOf[scalajs.js.Dynamic].scrollIntoView(scalajs.js.Dynamic.literal(behavior = "smooth"))
        ()
      }),
      onMouseOver.compose(_.sample(node)) --> (n =>
        c.onStateEvent.onNext(S.Event.HighlightEvent("enter", n.path, "ast"))
      ),
      onMouseOut.compose(_.sample(node)) --> (n =>
        c.onStateEvent.onNext(S.Event.HighlightEvent("leave", n.path, "ast"))
      ),
      onClick.compose(_.sample(node)) --> (n => c.onStateEvent.onNext(S.Event.HighlightEvent("click", n.path, "ast")))
    )

  private def guessSelfCollapsed(node: s3te.Ast.Node): Boolean =
    node.size == node.depth

  private def getRootType(tpe: s3te.ReflectionType): String =
    s3te.ReflectionType.parents(tpe) match
      case Nil    => tpe.toString
      case h :: _ => getRootType(h)
