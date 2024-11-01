package org.felher.s3te.ui.float

import org.felher.s3te.ui.geo.*

import com.raquo.laminar.api.L.*

class Calculator:
  private val calc = Var(Calculation(0, 0, "", "bottom"))
  val calcSignal   = calc.signal
  val arrowWidth   = 12

  def update(element: HtmlElement, state: Option[State]): Unit =
    state.foreach: state =>
      val areaNeeded       = Rect.fromElement(element.ref)
      val direction        = state.positioning.allowedDirections.maxBy(getDirectionScore(state.target, areaNeeded, _))
      val topLeft          = getTopLeft(state.target, areaNeeded, direction)
      val contentTransform = getContentTransform(topLeft, areaNeeded, direction)
      calc.set(
        Calculation(
          left = topLeft._1,
          top = topLeft._2,
          transform = contentTransform,
          arrowPos = direction match
            case Direction.Top    => "bottom"
            case Direction.Right  => "left"
            case Direction.Bottom => "top"
            case Direction.Left   => "right"
        )
      )

  private def getTopLeft(target: org.scalajs.dom.Element, requiredArea: Rect, dir: Direction): (Double, Double) =
    val tcx = target.getBoundingClientRect().left + target.getBoundingClientRect().width / 2
    val tcy = target.getBoundingClientRect().top + target.getBoundingClientRect().height / 2
    dir match
      case Direction.Top =>
        (
          tcx - requiredArea.width / 2,
          target.getBoundingClientRect().top - requiredArea.height
        )

      case Direction.Right =>
        (
          target.getBoundingClientRect().right,
          tcy - requiredArea.height / 2
        )

      case Direction.Bottom =>
        (
          tcx - requiredArea.width / 2,
          target.getBoundingClientRect().bottom
        )

      case Direction.Left =>
        (
          target.getBoundingClientRect().left - requiredArea.width,
          tcy - requiredArea.height / 2
        )

  private def getContentTransform(topLeft: (Double, Double), areaRequirement: Rect, dir: Direction): String =
    val (topLeftX, topLeftY) = topLeft
    val viewWidth            = org.scalajs.dom.window.innerWidth
    val viewHeight           = org.scalajs.dom.window.innerHeight

    val translationIsHorizontal = dir == Direction.Top || dir == Direction.Bottom

    if translationIsHorizontal then
      if topLeftX < 0 then s"transform: translateX(${topLeftX.abs}px);"
      else if topLeftX + areaRequirement.width > viewWidth then
        s"transform: translateX(${(viewWidth - topLeftX - areaRequirement.width)}px);"
      else ""
    else if topLeftY < 0 then s"transform: translateY(${topLeftY.abs}px);"
    else if topLeftY + areaRequirement.height > viewHeight then
      s"transform: translateY(${(viewHeight - topLeftY - areaRequirement.height)}px);"
    else ""

  private def getDirectionScore(target: org.scalajs.dom.Element, areaRequirement: Rect, dir: Direction) =
    val viewWidth  = org.scalajs.dom.window.innerWidth
    val viewHeight = org.scalajs.dom.window.innerHeight

    val freeSpace = dir match
      case Direction.Top    => Rect(Vec2(0, 0), Vec2(viewWidth, target.getBoundingClientRect().top))
      case Direction.Right  =>
        Rect(
          Vec2(target.getBoundingClientRect().right, 0),
          Vec2(viewWidth - target.getBoundingClientRect().right, viewHeight)
        )
      case Direction.Bottom =>
        Rect(
          Vec2(0, target.getBoundingClientRect().bottom),
          Vec2(viewWidth, viewHeight - target.getBoundingClientRect().bottom)
        )
      case Direction.Left   => Rect(Vec2(0, 0), Vec2(target.getBoundingClientRect().left, viewHeight))

    val intersection = freeSpace.intersection(areaRequirement.alignTopLeftTo(freeSpace))

    intersection.area
