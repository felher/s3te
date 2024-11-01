package org.felher.s3te

final case class Span(
    start: Int,
    end: Int
) derives JsonEncoder:
  require(start <= end, s"start($start) must be less than or equals to end($end)")
  def length: Int    = end - start

  def contains(other: Span): Boolean =
    start <= other.start && other.end <= end

  def empty: Boolean =
    start == end
