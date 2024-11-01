package org.felher.s3te.ui.extra

import org.felher.s3te.ui
import com.raquo.laminar.api.L.*
import org.felher.s3te
import org.felher.beminar.Bem

object Actions:
  val bem = Bem("/extra-actions")

  def render(ast: s3te.Ast): HtmlElement =
    div(
      bem,
      ast match
        case node: s3te.Ast.Node =>
          scaladocs(node.tpe)

        case _ => List.empty[HtmlElement]
    )

  private def scaladocs(tpe: s3te.ReflectionType): HtmlElement =
    val supertypes = allSupertypes(tpe).distinct.filterNot(_ == tpe).reverse
    val subtypes   = allSubtypes(tpe).distinct.filterNot(_ == tpe)

    div(
      bem("/scaladocs"),
      div(bem("/scaladocs-title"), "Supertypes"),
      supertypes.map(renderDocsForType),
      div(bem("/scaladocs-title"), "Current"),
      renderDocsForType(tpe),
      div(bem("/scaladocs-title"), "Subtypes"),
      subtypes.map(renderDocsForType)
    )

  private def renderDocsForType(tpe: s3te.ReflectionType): HtmlElement =
    div(
      bem("/scaladoc-link"),
      span(tpe.toString),
      a(
        href(getModuleLink(tpe)),
        div(bem("/scala-icon", "missing" -> !s3te.ReflectionType.hasModuleDoc(tpe), "object")),
        target("_blank")
      ),
      a(
        href(getMethodsLink(tpe)),
        div(bem("/scala-icon", "missing" -> !s3te.ReflectionType.hasMethodsDocs(tpe), "trait")),
        target("_blank")
      )
    )

  private def allSupertypes(tpe: s3te.ReflectionType): List[s3te.ReflectionType] =
    tpe :: s3te.ReflectionType.parents(tpe).flatMap(allSupertypes)

  private def allSubtypes(tpe: s3te.ReflectionType): List[s3te.ReflectionType] =
    val directChildren =
      s3te.ReflectionType.values.toList.filter(otherTpe => s3te.ReflectionType.parents(otherTpe).contains(tpe))
    tpe :: directChildren.flatMap(allSubtypes)

  private def getModuleLink(tpe: s3te.ReflectionType): String =
    s"https://dotty.epfl.ch/api/scala/quoted/Quotes${"$"}reflectModule${"$"}${tpe.toString + "Module"}.html"

  private def getMethodsLink(tpe: s3te.ReflectionType): String =
    s"https://dotty.epfl.ch/api/scala/quoted/Quotes${"$"}reflectModule${"$"}${tpe.toString + "Methods"}.html"
