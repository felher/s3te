package org.felher.s3te.ui.float

import com.raquo.laminar.api.L.*

final case class Calculation(
    left: Double,
    top: Double,
    transform: String,
    arrowPos: String
):
  def outerStyle: String = s"""
    left: ${left}px;
    top: ${top}px;
  """
