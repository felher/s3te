package org.felher.s3te.ui.state

import org.felher.s3te.ui
import org.felher.s3te

final case class State(
    code: String,
    whole: s3te.Ast,
    spans: Map[s3te.Path, s3te.Span],
    drill: s3te.Path,
    showParams: Boolean,
    showActions: Boolean,
    typeColors: Boolean,
    highlight: Option[Highlight]
):
  def handleEvent(event: Event): Either[String, State] =
    event match
      case Event.SetDrill(path) =>
        path.lookupIn(whole) match
          case s3te.LookupResult.NotFound              => Left(s"Path $path not found in AST")
          case s3te.LookupResult.FoundMethodLeaf(_, _) => Left(s"Path $path was method leaf")
          case s3te.LookupResult.FoundAst(_)           => Right(copy(drill = path))

      case Event.SetShowParams(show)  => Right(copy(showParams = show))
      case Event.SetShowActions(show) => Right(copy(showActions = show))
      case Event.SetTypeColors(show)  => Right(copy(typeColors = show))

      case he: Event.HighlightEvent =>
        Right(copy(highlight = highlight.fold(Highlight.initial(he))(h => h.handleEvent(spans, he))))

  lazy val root = drill.lookupIn(whole).asAst.getOrElse(sys.error(s"Root not found in AST for path $drill"))
