package org.felher.s3te.ui.geo

final case class Rect(
    topLeft: Vec2,
    dimensions: Vec2
):
  val width: Double  = dimensions.x
  val height: Double = dimensions.y
  def area: Double   = width * height
  val bottomRight    = Vec2(topLeft.x + width, topLeft.y + height)

  def alignTopLeftTo(other: Rect): Rect =
    Rect(
      topLeft = other.topLeft,
      dimensions = dimensions
    )

  def intersection(other: Rect): Rect =
    val topLeft     = this.topLeft.elementWiseMax(other.topLeft)
    val bottomRight = this.bottomRight.elementWiseMin(other.bottomRight)

    val brClamped = bottomRight.x max topLeft.x
    val trClamped = bottomRight.y max topLeft.y

    Rect(topLeft, Vec2(brClamped - topLeft.x, trClamped - topLeft.y))

  def contains(point: Vec2): Boolean =
    point.x >= topLeft.x && point.x <= bottomRight.x &&
      point.y >= topLeft.y && point.y <= bottomRight.y

  def move(delta: Vec2): Rect =
    Rect(topLeft + delta, dimensions)

  def resizeClamp(
      delta: Vec2,
      grabbedEdge: Direction,
      minDim: Vec2
  ): Rect =
    grabbedEdge match
      case Direction.Top =>
        val newTopLeft = topLeft + delta.zeroX
        val newDim     = (dimensions - delta.zeroX).elementWiseMax(minDim)
        Rect(newTopLeft, newDim)

      case Direction.Right =>
        if delta.x > 0 then Rect(topLeft, dimensions + delta.zeroY)
        else
          val allowedShrink = Math.max(0, dimensions.x - minDim.x)
          val shrink        = Math.min(-delta.x, allowedShrink)
          val movement      = -delta.x - shrink
          Rect(topLeft - Vec2(movement, 0), dimensions - Vec2(shrink, 0))

      case Direction.Bottom =>
        if delta.x > 0 then Rect(topLeft, dimensions + delta.zeroX)
        else
          val allowedShrink = Math.max(0, dimensions.y - minDim.y)
          val shrink        = Math.min(-delta.y, allowedShrink)
          val movement      = -delta.y - shrink
          Rect(topLeft - Vec2(0, movement), dimensions - Vec2(0, shrink))

      case Direction.Left =>
        val newTopLeft = topLeft + delta.zeroY
        val newDim     = (dimensions - delta.zeroY).elementWiseMax(minDim)
        Rect(newTopLeft, newDim)

object Rect:
  def fromElement(element: org.scalajs.dom.Element): Rect =
    val rect = element.getBoundingClientRect()
    Rect(Vec2(rect.left, rect.top), Vec2(rect.width, rect.height))
