package org.felher.s3te.ui

import org.felher.s3te.ui.geo.Vec2
import com.raquo.laminar.api.L.*

object Mover:
  def create(observer: Observer[Vec2]): Modifier.Base =
    var lastPos = Option.empty[Vec2]

    List(
      onPointerDown --> (ev => { lastPos = Some(Vec2(ev.clientX, ev.clientY)) }),
      windowEvents(_.onPointerUp) --> (_ => { lastPos = None }),
      documentEvents(_.onPointerMove) --> (event =>
        lastPos match
          case None => ()
          case Some(lp) =>
            val curPos = Vec2(event.clientX, event.clientY)
            val delta = curPos - lp
            lastPos = Some(curPos)
            observer.onNext(delta)
      )
    )
