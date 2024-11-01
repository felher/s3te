package org.felher.s3te.ui

import org.felher.s3te
import scala.util.chaining.*

object Spanner:
  def getPathMap(ast: s3te.Ast): Map[s3te.Path, s3te.Span] =
    def loop(ast: s3te.Ast): List[(s3te.Path, s3te.Span)] =
      ast match
        case n: s3te.Ast.Node       =>
          n.span.map(span => (n.path -> span)).toList ++ n.children.flatMap(child => loop(child))
        case c: s3te.Ast.Collection =>
          c.children.flatMap(child => loop(child))
        case _                      =>
          Nil
    loop(ast).toMap

  final case class Partial(
      text: String,
      span: s3te.Span,
      starts: Set[s3te.Path],
      paths: Set[s3te.Path],
      ends: Set[s3te.Path]
  )

  def split(text: String, spans: Map[s3te.Path, s3te.Span]): List[Partial] =
    execute(text, spans.toList).toList

  final private case class Point(
      idx: Int,
      starts: Set[s3te.Path],
      ends: Set[s3te.Path]
  ):
    def addStart(path: s3te.Path): Point = copy(starts = starts + path)
    def addEnd(path: s3te.Path): Point   = copy(ends = ends + path)

  def execute(text: String, spans: List[(s3te.Path, s3te.Span)]): Vector[Partial] =
    def emptyPoint(idx: Int) = Point(idx, Set.empty, Set.empty)

    val points =
      spans
        .foldLeft(Map.empty[Int, Point]): (m, ps) =>
          val (path, span) = ps
          m
            .updatedWith(span.start)(op => Some(op.getOrElse(emptyPoint(span.start)).addStart(path)))
            .updatedWith(span.end)(op => Some(op.getOrElse(emptyPoint(span.end)).addEnd(path)))
        .updatedWith(0)(op => Some(op.getOrElse(emptyPoint(0))))
        .updatedWith(text.length)(op => Some(op.getOrElse(emptyPoint(text.length))))

    fromPoints(text, points.values.toList.sortBy(_.idx), Set.empty, Vector.empty)

  private def fromPoints(
      text: String,
      points: List[Point],
      active: Set[s3te.Path],
      partials: Vector[Partial]
  ): Vector[Partial] =
    points match
      case Nil => partials

      case pLast :: Nil =>
        val createdAndClosed = pLast.starts.intersect(pLast.ends)
        if createdAndClosed.isEmpty then partials
        else
          partials :+ Partial("", s3te.Span(pLast.idx, pLast.idx), createdAndClosed, createdAndClosed, createdAndClosed)

      case p1 :: p2 :: tail =>
        val startAndEndsAtStart = p1.starts.intersect(p1.ends)
        val zeroWidthAtStart    =
          if startAndEndsAtStart.isEmpty then Vector.empty
          else
            Vector(
              Partial(
                "",
                s3te.Span(p1.idx, p1.idx),
                startAndEndsAtStart,
                startAndEndsAtStart ++ active,
                startAndEndsAtStart
              )
            )

        val activeWithin = active ++ p1.starts -- p1.ends
        val activeAfter  = activeWithin -- p2.ends

        val newPartial = Partial(
          text.substring(p1.idx, p2.idx),
          s3te.Span(p1.idx, p2.idx),
          p1.starts -- p1.ends,
          activeWithin,
          p2.ends -- p2.starts
        )

        fromPoints(
          text,
          p2 :: tail,
          activeAfter,
          partials ++ zeroWidthAtStart :+ newPartial
        )
