package org.felher.s3te.ui.dialog

import com.raquo.laminar.api.L.*
import org.felher.beminar.Bem
import org.scalajs.dom.window
import org.felher.s3te.ui.geo as G

class Dialog(val title: String, val content: HtmlElement, val manager: Dialogs):
  private val bem  = Dialog.bem
  private val area = Var(
    G.Rect(
      topLeft = G.Vec2(window.innerWidth / 4, window.innerHeight / 4),
      dimensions = G.Vec2(window.innerWidth / 2, window.innerHeight / 2)
    )
  )

  private val styleSig = area.signal.map(area =>
    s"width: ${area.width}px; height: ${area.height}px; left: ${area.topLeft.x}px; top: ${area.topLeft.y}px"
  )

  private lazy val move = Observer[G.Vec2]: delta =>
    area.update(_.move(delta))
    manager.moveToTop(this)

  lazy val element = div(
    idAttr(Math.random().toString),
    styleAttr <-- styleSig,
    bem,
    onClick.mapTo(this) --> manager.moveToTop,
    div(
      bem("/header"),
      org.felher.s3te.ui.Mover.create(move),
      div(bem("/title"), title),
      div(
        bem("/close"),
        onClick.mapTo(this) --> manager.close
      )
    ),
    content.amend(bem("/content")),
    List["w" | "e" | "s" | "sw" | "se"]("w", "e", "s", "sw", "se").map(edge =>
      div(
        bem("/resize-" + edge),
        resizer(area, edge)
      )
    )
  )

  private def resizer(
      area: Var[G.Rect],
      edge: "w" | "e" | "s" | "sw" | "se"
  ): Modifier.Base =
    val directions = edge match
      case "w"  => List(G.Direction.Left)
      case "e"  => List(G.Direction.Right)
      case "s"  => List(G.Direction.Bottom)
      case "sw" => List(G.Direction.Bottom, G.Direction.Left)
      case "se" => List(G.Direction.Bottom, G.Direction.Right)

    val bus = new EventBus[G.Vec2]
    List(
      org.felher.s3te.ui.Mover.create(bus.writer),
      bus.events --> (delta =>
        area.update(area =>
          directions.foldLeft(area): (area, dir) =>
            area.resizeClamp(
              delta,
              dir,
              G.Vec2(100, 100)
            )
        )
      )
    )

object Dialog:
  val bem = Bem("/dialog")
