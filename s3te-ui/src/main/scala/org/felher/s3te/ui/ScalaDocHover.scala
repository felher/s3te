package org.felher.s3te.ui

import float as F
import com.raquo.laminar.api.L.*
import org.felher.s3te
import org.felher.beminar.Bem

class ScalaDocHover:
  private val float = F.Float()

  def mount = float.mount

  def open(target: org.scalajs.dom.Element, tpe: s3te.ReflectionType): Unit =
    float.open(
      target,
      div(
        ScalaDocHover.bem("/content"),
        onMountCallback(ref => ref.thisNode.ref.innerHTML = s3te.ReflectionType.scalaDocShort(tpe))
      ),
      F.Positioning(
        List(
          geo.Direction.Top,
          geo.Direction.Bottom
        ),
        useOverlay = false
      )
    )

  def close(target: org.scalajs.dom.Element): Unit =
    float.close(target)

object ScalaDocHover:
  val bem = Bem("/scala-doc-hover")

  val instance: ScalaDocHover = ScalaDocHover()
