package org.felher.s3te.ui

import com.raquo.laminar.api.L.*
import org.felher.s3te
import org.felher.s3te.ui.state as S
import extra as E

final case class Context(
    hover: ScalaDocHover,
    extra: E.Extra,
    parentIsHighlighted: Signal[Boolean],
    parentIsCollapsed: Signal[Boolean],
    state: Signal[S.State],
    onStateEvent: Observer[S.Event]
):
  def withHighlight(highlight: Signal[Boolean]): Context = copy(parentIsHighlighted = highlight)
  def withCollapse(collapse: Signal[Boolean]): Context   = copy(parentIsCollapsed = collapse)

object Context:
  def start(
      hover: ScalaDocHover,
      extra: E.Extra,
      state: Signal[S.State],
      onStateEvent: Observer[S.Event]
  ): Context =
    Context(
      hover,
      extra,
      Signal.fromValue(false),
      Signal.fromValue(false),
      state,
      onStateEvent
    )
