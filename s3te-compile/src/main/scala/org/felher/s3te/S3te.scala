package org.felher.s3te
import scala.quoted.*
import java.nio.charset.StandardCharsets

object S3te:
  val indexTemplate =
    try {
      val inputStream = getClass().getResourceAsStream("/org/felher/s3te/index.html")
      val string      = String(inputStream.readAllBytes(), StandardCharsets.UTF_8)
      inputStream.close()
      string
    } catch {
      case e: Exception =>
        throw new Exception("Failed to load /org/felher/s3te/index.html", e)
    }

  def renderTreeHTML(using q: Quotes)(path: String, t: q.reflect.Tree): Unit =
    val creator = AstCreator(using q)
    val ast     = creator.visitTree(t)(using Path.empty)
    val json    = JsonEncoder.encode(TreeData(t.pos.sourceFile.content, ast))
    val html    = indexTemplate.replace("\"INSERT_DATA_HERE\"", json)
    java.nio.file.Files.writeString(java.nio.file.Paths.get(path), html, StandardCharsets.UTF_8)
    ()

  def renderTreeJSON(using q: Quotes)(path: String, t: q.reflect.Tree): Unit =
    val creator = AstCreator(using q)
    val ast     = creator.visitTree(t)(using Path.empty)
    val json    = JsonEncoder.encode(TreeData(t.pos.sourceFile.content, ast))
    java.nio.file.Files.writeString(java.nio.file.Paths.get(path), json, StandardCharsets.UTF_8)
    ()
