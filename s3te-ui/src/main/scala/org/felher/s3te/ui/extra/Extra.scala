package org.felher.s3te.ui.extra

import org.felher.s3te.ui
import org.felher.s3te.ui.dialog as D
import com.raquo.laminar.api.L.*
import org.felher.s3te
import org.felher.beminar.Bem
import org.felher.s3te.Ast

class Extra:
  import Extra.*

  private val float = D.Dialogs()

  def mount: HtmlElement = float.mount

  private def render(whole: s3te.Ast, initialDrill: s3te.Path): HtmlElement =
    val current = initialDrill.lookupIn(whole).asAst.getOrElse(sys.error("whole doesn't contain initialDrill"))
    div(
      bem("/content"),
      div(bem("/scala-docs-title"), "Scala Docs"),
      Actions.render(current).amend(bem("/actions")),
      ui.Tabs
        .render(
          Signal.fromValue(
            TabLongScaladoc.render(current).toList ++
              TabAstMethod.render(whole, initialDrill, s3te.MethodKey.DotTpe).toList ++
              TabAstMethod.render(whole, initialDrill, s3te.MethodKey.DotSymbol).toList ++
              TabSymbolFlagsMethod.render(current).toList ++
              TabPos.render(current).toList ++
              TabExtractor.render(whole, initialDrill).toList
          )
        )
        .amend(bem("/tabs"), ui.Tabs.bemSubgrid)
    )

  private def getTitle(ast: s3te.Ast): String =
    ast match
      case n: Ast.Node       => n.tpe.toString
      case n: Ast.Collection => n.name
      case n: Ast.Leaf       => n.raw
      case n: Ast.Reference  => "Reference"

  def open(whole: s3te.Ast, initialDrill: s3te.Path): Unit =
    val current = initialDrill.lookupIn(whole).asAst.getOrElse(sys.error("whole doesn't contain initialDrill"))
    float.open(
      getTitle(current),
      render(whole, initialDrill)
    )

object Extra:
  val bem = Bem("/extra")

  val instance = Extra()

  class Dummy extends Extra:
    override def mount: HtmlElement                                 = div()
    override def open(ast: s3te.Ast, initialDrill: s3te.Path): Unit = ()
