package org.felher.s3te.ui

import org.felher.s3te
import org.felher.s3te.ui.state as S
import org.felher.beminar.Bem
import com.raquo.laminar.api.L.*

object Header:
  val bem = Bem("/header")

  def render(state: Signal[S.State], onStateEvent: Observer[S.Event]): HtmlElement =
    div(
      bem,
      child <-- state.map: state =>
        div(
          bem("/breadcrumbs"),
          getBreadcrumbs(state.whole, state.drill).map(bc =>
            div(
              bem("/breadcrumb"),
              bc.name,
              onClick --> (_ => onStateEvent.onNext(S.Event.SetDrill(bc.path)))
            )
          )
        ),
      div(
        bem("/config-property"),
        div(bem("/config-property-name"), "Show Params"),
        input(
          tpe("checkbox"),
          controlled(
            checked <-- state.map(_.showParams),
            onClick.mapToChecked --> (show => onStateEvent.onNext(S.Event.SetShowParams(show)))
          )
        )
      ),
      div(
        bem("/config-property"),
        div(bem("/config-property-name"), "Show Actions"),
        input(
          tpe("checkbox"),
          controlled(
            checked <-- state.map(_.showActions),
            onClick.mapToChecked --> (show => onStateEvent.onNext(S.Event.SetShowActions(show)))
          )
        )
      ),
      div(
        bem("/config-property"),
        div(bem("/config-property-name"), "Type Colors"),
        input(
          tpe("checkbox"),
          controlled(
            checked <-- state.map(_.typeColors),
            onClick.mapToChecked --> (show => onStateEvent.onNext(S.Event.SetTypeColors(show)))
          )
        )
      )
    )

  final case class Breadcrumb(
      name: String,
      path: s3te.Path
  )

  private def getBreadcrumbs(ast: s3te.Ast, path: s3te.Path): List[Breadcrumb] =
    path.allInits.flatMap: p =>
      p.lookupIn(ast) match
        case s3te.LookupResult.FoundAst(n: s3te.Ast.Node) => List(Breadcrumb(n.tpe.toString, p))
        case s3te.LookupResult.FoundAst(_)                => Nil
        case s3te.LookupResult.FoundMethodLeaf(_, _)                   => sys.error("Invalid path while constructing breadcrumbs")
        case s3te.LookupResult.NotFound                   => sys.error("Invalid path while constructing breadcrumbs")
