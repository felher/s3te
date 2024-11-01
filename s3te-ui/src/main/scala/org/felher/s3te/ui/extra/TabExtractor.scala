package org.felher.s3te.ui.extra

import org.felher.s3te.ui
import com.raquo.laminar.api.L.*
import org.felher.s3te

object TabExtractor:
  def render(whole: s3te.Ast, drill: s3te.Path): Option[(String, HtmlElement)] =
    drill.lookupIn(whole) match
      case s3te.LookupResult.NotFound              => None
      case s3te.LookupResult.FoundMethodLeaf(_, _) => None
      case s3te.LookupResult.FoundAst(_)           =>
        Some(
          "Extractor",
          pre(
            buildExtractor("expr.asTerm", "", whole, drill).map(_.mkString("\n")) match
              case Right(s)  => s
              case Left(err) => err
          )
        )

  private def buildExtractor(
      valueToMatch: String,
      indent: String,
      current: s3te.Ast,
      path: s3te.Path
  ): Either[String, List[String]] =
    path.splitHead match
      case None               => Right(List(indent + valueToMatch))
      case Some((head, path)) =>
        (head, current) match
          case (s3te.PathEntry.MethodCall(k), s3te.Ast.Node(_, _, _, _, mm))                 =>
            k.asAstKey match
              case None    => Left(s"Method key $k is not an Ast key")
              case Some(k) =>
                mm.get(k) match
                  case None    => Left(s"Method key $k not found in method data")
                  case Some(v) => buildExtractor(s"$valueToMatch${k.methodSyntaxName}", indent, v, path)
          case (s3te.PathEntry.MethodCall(_), s3te.Ast.Reference(_, _))                      => Left("Can not extract method from reference")
          case (s3te.PathEntry.MethodCall(_), s3te.Ast.Leaf(_, _))                           => Left("Can not extract method from leaf")
          case (s3te.PathEntry.MethodCall(_), s3te.Ast.Collection(_, _, _))                  =>
            Left("Can not extract method from collection")
          case (s3te.PathEntry.ExtractorChild(index), s3te.Ast.Reference(_, _))              =>
            Left("Can not extract from reference")
          case (s3te.PathEntry.ExtractorChild(index), s3te.Ast.Leaf(_, _))                   => Left("Can not extract from leaf")
          case (s3te.PathEntry.ExtractorChild(index), s3te.Ast.Node(_, tpe, _, children, _)) =>
            s3te.ReflectionType.extractorNames(tpe) match
              case None                 => Left(s"type $tpe has no extractor names defined")
              case Some(extractorNames) =>
                extractorNames.lift(index) match
                  case None            => Left(s"type $tpe has no extractor name at index $index")
                  case Some(extractor) =>
                    val before     = List.fill(index)("_")
                    val after      = List.fill(children.size - index - 1)("_")
                    val extractors = (before :+ extractor :++ after).mkString(", ")
                    val l1         = s"$indent$valueToMatch match"
                    val l2         = s"$indent  case ${tpe.toString}($extractors) =>"
                    buildExtractor(extractor, indent + "    ", children(index), path).map(l1 +: l2 +: _)

          case (s3te.PathEntry.ExtractorChild(index), s3te.Ast.Collection(_, _, children)) =>
            children.lift(index) match
              case None        => Left(s"Collection has no child at index $index")
              case Some(child) => buildExtractor(s"$valueToMatch($index)", indent, child, path)
