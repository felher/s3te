package org.felher.s3te.ui.extra

import org.felher.s3te.ui
import com.raquo.laminar.api.L.*
import org.felher.s3te

object TabLongScaladoc:
  def render(ast: s3te.Ast): Option[(String, HtmlElement)] =
    ast match
      case node: s3te.Ast.Node =>
        Some(
          "Scala Doc",
          div(onMountCallback(el => el.thisNode.ref.innerHTML = s3te.ReflectionType.scalaDoc(node.tpe)))
        )

      case _ => None
