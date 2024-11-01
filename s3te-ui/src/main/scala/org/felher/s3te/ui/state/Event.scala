package org.felher.s3te.ui.state

import org.felher.s3te.ui
import org.felher.s3te

enum Event:
  case SetDrill(path: s3te.Path)
  case SetShowParams(show: Boolean)
  case SetShowActions(show: Boolean)
  case SetTypeColors(show: Boolean)

  case HighlightEvent(
    tpe: "click" | "enter" | "leave",
    path: s3te.Path,
    source: "ast" | "code"
  )
