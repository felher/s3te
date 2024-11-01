package org.felher.s3te.ui.geo

final case class Vec2(x: Double, y: Double):
  def elementWiseMax(other: Vec2): Vec2 = Vec2(x.max(other.x), y.max(other.y))

  def elementWiseMin(other: Vec2): Vec2 = Vec2(x.min(other.x), y.min(other.y))

  def clampBetween(min: Vec2, max: Vec2): Vec2 =
    val newX = x.max(min.x).min(max.x)
    val newY = y.max(min.y).min(max.y)
    Vec2(newX, newY)

  def +(other: Vec2): Vec2 = Vec2(x + other.x, y + other.y)
  def -(other: Vec2): Vec2 = Vec2(x - other.x, y - other.y)
  def zeroX: Vec2         = Vec2(0, y)
  def zeroY: Vec2         = Vec2(x, 0)
