package org.felher.s3te.ui

import org.felher.s3te
import io.circe.*
import org.felher.s3te.PathEntry

object AstDecoder:
  given Decoder[s3te.ReflectionType] = io.circe.Decoder.decodeString.emapTry: s =>
    scala.util.Try:
      s3te.ReflectionType.valueOf(s)

  given Decoder[s3te.Span]         = io.circe.generic.semiauto.deriveDecoder[s3te.Span]
  given Decoder[s3te.MethodKey[?]] = io.circe.Decoder.decodeString.emapTry: s =>
    scala.util.Try:
      s3te.MethodKey.valueOf(s)
  given Decoder[s3te.Flag] = io.circe.Decoder.decodeString.emapTry: s =>
    scala.util.Try:
      s3te.Flag.valueOf(s)
  given Decoder[s3te.PathEntry]    = io.circe.Decoder.decodeJsonObject.flatMap: obj =>
    if obj.contains("index") then
      Decoder.instance(c => c.downField("index").as[Int].map(s3te.PathEntry.ExtractorChild.apply))
    else if obj.contains("key") then
      Decoder.instance(c => c.downField("key").as[s3te.MethodKey[?]].map(s3te.PathEntry.MethodCall.apply))
    else Decoder.failedWithMessage("Unknown PathEntry type")

  given Decoder[s3te.Path] = io.circe.Decoder.decodeList[PathEntry].asInstanceOf[Decoder[s3te.Path]]

  def methodKeyValueDecoder[V](m: s3te.MethodKey[V]): Decoder[V] = m match
    case s3te.MethodKey.DotTpe            => summon[Decoder[s3te.Ast]]
    case s3te.MethodKey.DotSymbol         => summon[Decoder[s3te.Ast]]
    case s3te.MethodKey.DotSymbolDotFlags => summon[Decoder[List[s3te.Flag]]]

  given Decoder[s3te.MethodMap] = io.circe.Decoder.decodeJsonObject.flatMap: obj =>
    val keys                                                                = obj.keys.toList
    def decodeAll(keys: List[String]): Decoder[Map[s3te.MethodKey[?], Any]] =
      keys match
        case Nil         => Decoder.const(Map.empty)
        case key :: keys =>
          val methodKey = s3te.MethodKey.valueOf(key)
          val valueDec  = methodKeyValueDecoder(methodKey)
          valueDec.at(key).flatMap(v => decodeAll(keys).map(m => m + (methodKey -> v)))

    decodeAll(keys).map(m => s3te.MethodMap(m))

  given Decoder[s3te.Ast] =
    Decoder.decodeJsonObject.flatMap: obj =>
      if obj.contains("tpe") then decodeNode.map(a => a: s3te.Ast)
      else if obj.contains("name") then decodeCollection.map(a => a: s3te.Ast)
      else if obj.contains("raw") then decodeLeaf.map(a => a: s3te.Ast)
      else if obj.contains("target") then decodeReference.map(a => a: s3te.Ast)
      else Decoder.failedWithMessage("Unknown Ast type: " + obj.toString)

  private def decodeNode: Decoder[s3te.Ast.Node] = Decoder.instance: c =>
    for
      path       <- c.downField("path").as[s3te.Path]
      rt         <- c.downField("tpe").as[s3te.ReflectionType]
      span       <- c.downField("span").as[Option[s3te.Span]]
      children   <- c.downField("children").as[List[s3te.Ast]]
      methodData <- c.downField("methodData").as[s3te.MethodMap]
    yield s3te.Ast.Node(path, rt, span, children, methodData)

  private def decodeCollection: Decoder[s3te.Ast.Collection] = Decoder.instance: c =>
    for
      path     <- c.downField("path").as[s3te.Path]
      name     <- c.downField("name").as[String]
      children <- c.downField("children").as[List[s3te.Ast]]
    yield s3te.Ast.Collection(path, name, children)

  private def decodeLeaf: Decoder[s3te.Ast.Leaf] =
    Decoder.instance: c =>
      for
        path <- c.downField("path").as[s3te.Path]
        raw  <- c.downField("raw").as[String]
      yield s3te.Ast.Leaf(path, raw)

  private def decodeReference: Decoder[s3te.Ast.Reference] =
    Decoder.instance: c =>
      for
        path   <- c.downField("path").as[s3te.Path]
        target <- c.downField("target").as[s3te.Path]
      yield s3te.Ast.Reference(path, target)

  given Decoder[s3te.TreeData] = io.circe.generic.semiauto.deriveDecoder[s3te.TreeData]
