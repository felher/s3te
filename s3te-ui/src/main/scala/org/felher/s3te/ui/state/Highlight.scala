package org.felher.s3te.ui.state

import org.felher.s3te.ui
import org.felher.s3te

final case class Highlight(
    path: s3te.Path,
    isClick: Boolean,
    source: "ast" | "code"
):
  def handleEvent(spans: Map[s3te.Path, s3te.Span], event: Event.HighlightEvent): Option[Highlight] =
    event.tpe match
      case "leave" => None
      case "enter" => Some(Highlight(event.path, false, event.source))
      case "click" =>
        if !isClick then Some(Highlight(event.path, true, event.source))
        else
          Highlight.getSmallestEnclosingPath(spans, path) match
            case None          => Some(Highlight(event.path, true, event.source))
            case Some(newPath) => Some(Highlight(newPath, true, event.source))

object Highlight:
  def initial(event: Event.HighlightEvent): Option[Highlight] =
    event.tpe match
      case "leave" => None
      case "click" => Some(Highlight(event.path, isClick = true, source = event.source))
      case "enter" => Some(Highlight(event.path, isClick = false, source = event.source))

  private def getSmallestEnclosingPath(spans: Map[s3te.Path, s3te.Span], path: s3te.Path): Option[s3te.Path] =
    val next = path.dropRight(1)
    if spans.contains(next) then Some(next)
    else getSmallestEnclosingPath(spans, next)
