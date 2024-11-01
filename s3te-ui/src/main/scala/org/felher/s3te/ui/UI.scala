package org.felher.s3te.ui

import org.felher.s3te
import com.raquo.laminar.api.L.*
import scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.felher.beminar.Bem
import AstDecoder.given
import scala.concurrent.Future

object UI:
  @main def main(): Unit =
    val bem = Bem("/app")

    getAstData().onComplete:
      case scala.util.Failure(exception) =>
        org.scalajs.dom.window.alert("Could not fetch JSON")
        println(exception)
      case scala.util.Success(text)      =>
        io.circe.scalajs.decodeJs[s3te.TreeData](text) match
          case Left(err)       =>
            org.scalajs.dom.window.alert("Could not decode JSON")
            println(err)
          case Right(treeData) =>
            val (code, spans, partials) = treeData.code match
              case None       => ("", Map.empty, List.empty)
              case Some(code) =>
                val spans    = Spanner.getPathMap(treeData.ast)
                val partials = Spanner.split(code, spans)
                (code, spans, partials)

            val state        = Var(
              org.felher.s3te.ui.state.State(
                code = code,
                whole = treeData.ast,
                spans = spans,
                drill = s3te.Path.empty,
                showParams = true,
                showActions = true,
                typeColors = true,
                highlight = None
              )
            )
            val onStateEvent = Observer: (e: org.felher.s3te.ui.state.Event) =>
              val newState = state.now().handleEvent(e)
              newState match
                case Left(msg)       => org.scalajs.dom.window.alert(msg)
                case Right(newState) => state.set(newState)

            render(
              org.scalajs.dom.document.body,
              div(
                bem,
                ScalaDocHover.instance.mount,
                extra.Extra.instance.mount,
                Header.render(state.signal, onStateEvent).amend(bem("/header")),
                div(
                  bem("/tree"),
                  child <-- ast.Ast
                    .render(
                      state.signal.map(_.root).distinctByRef,
                      Context.start(ScalaDocHover.instance, extra.Extra.instance, state.signal, onStateEvent)
                    )
                ),
                div(
                  bem("/handle-bar"),
                  div(bem("/handle-bar-handle")),
                  div(bem("/handle-bar-handle")),
                  Mover.create(Observer(vec =>
                    val cssRootScope          =
                      org.scalajs.dom.document.querySelector(":root").asInstanceOf[org.scalajs.dom.HTMLElement]
                    val currentStyle          = org.scalajs.dom.window.getComputedStyle(cssRootScope)
                    val currentCodeSizeString = currentStyle.getPropertyValue("--code-size")
                    val percent               = currentCodeSizeString.stripSuffix("vw").toDouble
                    val windowWidth           = org.scalajs.dom.window.innerWidth
                    val deltaChange           = vec.x / windowWidth * 100
                    val newPercent            = percent + deltaChange
                    cssRootScope.style.setProperty("--code-size", s"${newPercent}vw")
                  ))
                ),
                div(
                  bem("/code"),
                  Code.render(partials, state.signal, onStateEvent)
                )
              )
            )

  private def getAstData(): Future[scala.scalajs.js.Any] =
    if scala.scalajs.js.typeOf(scala.scalajs.js.Dynamic.global.s3teData) == "object" then
      Future.successful(
        scala.scalajs.js.Dynamic.global.s3teData
      )
    else org.scalajs.dom.window.fetch("/dummy.json").toFuture.flatMap(_.json().toFuture)
