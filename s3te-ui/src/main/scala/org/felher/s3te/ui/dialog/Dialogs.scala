package org.felher.s3te.ui.dialog

import com.raquo.laminar.api.L.*
import org.felher.beminar.Bem

class Dialogs:
  private val bem     = Dialogs.bem
  private val dialogs = Var(List.empty[Dialog])

  val mount = div(
    bem,
    children <-- dialogs.signal.map(_.map(_.element))
  )

  def open(title: String, content: HtmlElement): Unit =
    val dialog = Dialog(title, content, this)
    dialogs.update(_ :+ dialog)
    org.scalajs.dom.window.setTimeout(() => moveToTop(dialog), 0)
    ()

  def close(dialog: Dialog): Unit =
    dialogs.update(_.filterNot(_ eq dialog))

  def moveToTop(dialog: Dialog): Unit =
    if dialogs.now().lastOption.contains(dialog) then ()
    else dialogs.update(_.filterNot(_ eq dialog) :+ dialog)

object Dialogs:
  val bem               = Bem("/dialogs")
  val instance: Dialogs = Dialogs()
