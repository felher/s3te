package org.felher.s3te

import scala.quoted.*

private class AstCreator[Q <: Quotes & Singleton](using val quotes: Q):
  import quotes.reflect.*
  var found = Map.empty[Any, Path]

  private trait Constructor[-T]:
    def construct(t: T): Path ?=> Ast

  private def path: Path ?=> Path = summon

  private object Constructor:
    given treeConstructor: Constructor[Tree] with
      def construct(t: Tree): Path ?=> Ast = visitTree(t)

    given stringConstructor: Constructor[String] with
      def construct(t: String): Path ?=> Ast = Ast.Leaf(path, Util.enquote(t))

    given primitiveConstructor: Constructor[Int | Long | Short | Byte | Char | Boolean | Double | Float] with
      def construct(t: Int | Long | Short | Byte | Char | Boolean | Double | Float): Path ?=> Ast =
        Ast.Leaf(path, t.toString)

    given constantConstructor: Constructor[Constant] with
      def construct(t: Constant): Path ?=> Ast = visitConstant(t)

    given typeReprConstructor: Constructor[TypeRepr] with
      def construct(t: TypeRepr): Path ?=> Ast = visitTypeRepr(t)

    given symbolConstructor: Constructor[Symbol] with
      def construct(t: Symbol): Path ?=> Ast = visitSymbol(t)

    given selectorConstructor: Constructor[Selector] with
      def construct(t: Selector): Path ?=> Ast = visitSelector(t)

    given paramClauseConstructor: Constructor[ParamClause] with
      def construct(t: ParamClause): Path ?=> Ast = visitParamClause(t)

    given optionConstructor[A: Constructor]: Constructor[Option[A]] with
      def construct(t: Option[A]): Path ?=> Ast =
        val p = path
        Ast.Collection(path, "Option", t.map(a => summon[Constructor[A]].construct(a)(using p / 0)).toList)

    given listConstrutor[A: Constructor]: Constructor[List[A]] with
      def construct(t: List[A]): Path ?=> Ast =
        val p = path
        Ast.Collection(
          path,
          "List",
          t.zipWithIndex.map((a, i) => summon[Constructor[A]].construct(a)(using p / i))
        )

  private def c[T: Constructor](t: T): Path ?=> Ast =
    found.get(t) match
      case Some(target) => Ast.Reference(path, target)
      case None         =>
        found = found + (t -> path)
        val ret = summon[Constructor[T]].construct(t)
        found = found - t
        ret

  private def node(tpe: ReflectionType, params: (Path ?=> Ast)*): Path ?=> Ast.Node =
    val p        = path
    val asFunctions = params.toList.map((p: (Path ?=> Ast)) => (path: Path) => p(using(path)))
    val withPath = asFunctions.zipWithIndex.map((param, idx) => param((p / idx)))
    if withPath.headOption.exists(_.path == p) then throw new Exception("Path is the same as the parent path")
    Ast.Node(p, tpe, None, withPath.toList, MethodMap.empty)

  private def visitSymbol(x: Symbol): Path ?=> Ast =
    val text =
      if x.isPackageDef then "IsPackageDefSymbol(<" + x.fullName + ">)"
      else if x.isClassDef then "IsClassDefSymbol(<" + x.fullName + ">)"
      else if x.isDefDef then "IsDefDefSymbol(<" + x.fullName + ">)"
      else if x.isValDef then "IsValDefSymbol(<" + x.fullName + ">)"
      else if x.isTypeDef then "IsTypeDefSymbol(<" + x.fullName + ">)"
      else if x.isNoSymbol then "NoSymbol()"
      else "UnknownSymbol(<" + x.fullName + ">)"

    node(ReflectionType.Symbol, Ast.Leaf(path, text))

  def visitParamClause(paramClause: ParamClause): Path ?=> Ast =
    paramClause match
      case TypeParamClause(params) =>
        node(ReflectionType.TypeParamClause, c(params))
      case TermParamClause(params) =>
        node(ReflectionType.TermParamClause, c(params))
      case _                       =>
        Ast.Leaf(path, "UnknownParamClause: " + paramClause)

  def visitSelector(selector: Selector): Path ?=> Ast =
    selector match
      case SimpleSelector(id)       =>
        node(ReflectionType.SimpleSelector, c(id))
      case OmitSelector(id)         =>
        node(ReflectionType.OmitSelector, c(id))
      case RenameSelector(id1, id2) =>
        node(ReflectionType.RenameSelector, c(id1), c(id2))
      case GivenSelector(bound)     =>
        node(ReflectionType.GivenSelector, c(bound))
      case _                        =>
        Ast.Leaf(path, "UnknownSelector: " + selector)

  @scala.annotation.nowarn()
  def visitTypeRepr(typeRepr: TypeRepr): Path ?=> Ast =
    typeRepr match
      case NoPrefix()                               =>
        node(ReflectionType.NoPrefix)
      case ThisType(tp)                             =>
        node(ReflectionType.ThisType, c(tp))
      case RecursiveType(underlying)                =>
        node(ReflectionType.RecursiveType, c(underlying))
      case OrType(left, right)                      =>
        node(ReflectionType.OrType, c(left), c(right))
      case RecursiveThis(binder)                    =>
        node(ReflectionType.RecursiveThis, c(binder))
      case AppliedType(tycon, args)                 =>
        node(ReflectionType.AppliedType, c(tycon), c(args))
      case TypeBounds(lo, hi)                       =>
        node(ReflectionType.TypeBounds, c(lo), c(hi))
      case AnnotatedType(underlying, annot)         =>
        node(ReflectionType.AnnotatedType, c(underlying), c(annot))
      case TypeRef(qual, name)                      =>
        node(ReflectionType.TypeRef, c(qual), c(name))
      case MatchCase(pat, rhs)                      =>
        node(ReflectionType.MatchCase, c(pat), c(rhs))
      case AndType(left, right)                     =>
        node(ReflectionType.AndType, c(left), c(right))
      case FlexibleType(tp)                         =>
        node(ReflectionType.FlexibleType, c(tp))
      case ByNameType(underlying)                   =>
        node(ReflectionType.ByNameType, c(underlying))
      case ParamRef(binder, idx)                    =>
        node(ReflectionType.ParamRef, c(binder), c(idx))
      case MatchType(bound, scrutinee, cases)       =>
        node(ReflectionType.MatchType, c(bound), c(scrutinee), c(cases))
      case TypeLambda(argNames, argBounds, resType) =>
        node(ReflectionType.TypeLambda, c(argNames), c(argBounds), c(resType))
      case PolyType(argNames, argBounds, resType)   =>
        node(ReflectionType.PolyType, c(argNames), c(argBounds), c(resType))
      case MethodType(argNames, argTypes, resType)  =>
        node(ReflectionType.MethodType, c(argNames), c(argTypes), c(resType))
      case SuperType(thistpe, supertpe)             =>
        node(ReflectionType.SuperType, c(thistpe), c(supertpe))
      case Refinement(parent, name, info)           =>
        node(ReflectionType.Refinement, c(parent), c(name), c(info))
      case TermRef(qual, name)                      =>
        node(ReflectionType.TermRef, c(qual), c(name))
      case ConstantType(value)                      =>
        node(ReflectionType.ConstantType, c(value))
      case _                                        =>
        Ast.Leaf(path, "UnknownTypeRepr: " + typeRepr)

  def visitConstant(constant: Constant): Path ?=> Ast =
    constant match
      case ShortConstant(value)   =>
        node(ReflectionType.ShortConstant, c(value))
      case ClassOfConstant(value) =>
        node(ReflectionType.ClassOfConstant, c(value))
      case NullConstant()         =>
        node(ReflectionType.NullConstant)
      case CharConstant(value)    =>
        node(ReflectionType.CharConstant, c(value))
      case IntConstant(value)     =>
        node(ReflectionType.IntConstant, c(value))
      case BooleanConstant(value) =>
        node(ReflectionType.BooleanConstant, c(value))
      case ByteConstant(value)    =>
        node(ReflectionType.ByteConstant, c(value))
      case DoubleConstant(value)  =>
        node(ReflectionType.DoubleConstant, c(value))
      case UnitConstant()         =>
        node(ReflectionType.UnitConstant)
      case FloatConstant(value)   =>
        node(ReflectionType.FloatConstant, c(value))
      case LongConstant(value)    =>
        node(ReflectionType.LongConstant, c(value))
      case StringConstant(value)  =>
        node(ReflectionType.StringConstant, c(value))
      case _                      =>
        Ast.Leaf(path, "UnknownConstant: " + constant)

  def visitTree(tree: Tree): Path ?=> Ast =
    val ast = tree match
      case TypeIdent(name)                             =>
        node(ReflectionType.TypeIdent, c(name))
      case Return(expr, from)                          =>
        node(ReflectionType.Return, c(expr), c(from))
      case Try(block, handlers, finalizer)             =>
        node(ReflectionType.Try, c(block), c(handlers), c(finalizer))
      case MatchTypeTree(bound, selector, cases)       =>
        node(ReflectionType.MatchTypeTree, c(bound), c(selector), c(cases))
      case If(cond, thenp, elsep)                      =>
        node(ReflectionType.If, c(cond), c(thenp), c(elsep))
      case Super(qual, mix)                            =>
        node(ReflectionType.Super, c(qual), c(mix))
      case TypeSelect(qualifier, name)                 =>
        node(ReflectionType.TypeSelect, c(qualifier), c(name))
      case TypeCaseDef(pat, body)                      =>
        node(ReflectionType.TypeCaseDef, c(pat), c(body))
      case TypeProjection(qualifier, name)             =>
        node(ReflectionType.TypeProjection, c(qualifier), c(name))
      case Repeated(elems, elemtpt)                    =>
        node(ReflectionType.Repeated, c(elems), c(elemtpt))
      case Typed(expr, tpt)                            =>
        node(ReflectionType.Typed, c(expr), c(tpt))
      case Annotated(arg, annot)                       =>
        node(ReflectionType.Annotated, c(arg), c(annot))
      case This(qual)                                  =>
        node(ReflectionType.This, c(qual))
      case Singleton(ref)                              =>
        node(ReflectionType.Singleton, c(ref))
      case PackageClause(pid, stats)                   =>
        node(ReflectionType.PackageClause, c(pid), c(stats))
      case CaseDef(pat, guard, body)                   =>
        node(ReflectionType.CaseDef, c(pat), c(guard), c(body))
      case Inlined(call, bindings, expansion)          =>
        node(ReflectionType.Inlined, c(call), c(bindings), c(expansion))
      case DefDef(name, paramsClauses, returnTpt, rhs) =>
        node(ReflectionType.DefDef, c(name), c(paramsClauses), c(returnTpt), c(rhs))
      case ClassDef(name, constr, parents, self, body) =>
        node(ReflectionType.ClassDef, c(name), c(constr), c(parents), c(self), c(body))
      case Export(expr, selectors)                     =>
        node(ReflectionType.Export, c(expr), c(selectors))
      case WildcardTypeTree()                          =>
        node(ReflectionType.WildcardTypeTree)
      case TypedOrTest(tree, tpt)                      =>
        node(ReflectionType.TypedOrTest, c(tree), c(tpt))
      case TypeApply(fun, args)                        =>
        node(ReflectionType.TypeApply, c(fun), c(args))
      case ByName(result)                              =>
        node(ReflectionType.ByName, c(result))
      case Literal(const)                              =>
        node(ReflectionType.Literal, c(const))
      case Inferred()                                  =>
        node(ReflectionType.Inferred)
      case LambdaTypeTree(tparams, body)               =>
        node(ReflectionType.LambdaTypeTree, c(tparams), c(body))
      case Alternatives(patterns)                      =>
        node(ReflectionType.Alternatives, c(patterns))
      case TypeBind(name, bounds)                      =>
        node(ReflectionType.TypeBind, c(name), c(bounds))
      case TypeDef(name, rhs)                          =>
        node(ReflectionType.TypeDef, c(name), c(rhs))
      case Apply(fun, args)                            =>
        node(ReflectionType.Apply, c(fun), c(args))
      case Applied(tpt, args)                          =>
        node(ReflectionType.Applied, c(tpt), c(args))
      case TypeBoundsTree(lo, hi)                      =>
        node(ReflectionType.TypeBoundsTree, c(lo), c(hi))
      case Wildcard()                                  =>
        node(ReflectionType.Wildcard)
      case While(cond, body)                           =>
        node(ReflectionType.While, c(cond), c(body))
      case Unapply(fun, implicits, patterns)           =>
        node(ReflectionType.Unapply, c(fun), c(implicits), c(patterns))
      case Import(expr, selectors)                     =>
        node(ReflectionType.Import, c(expr), c(selectors))
      case Refined(tpt, refinements)                   =>
        node(ReflectionType.Refined, c(tpt), c(refinements))
      case SummonFrom(cases)                           =>
        node(ReflectionType.SummonFrom, c(cases))
      case Bind(name, body)                            =>
        node(ReflectionType.Bind, c(name), c(body))
      case ValDef(name, tpt, rhs)                      =>
        node(ReflectionType.ValDef, c(name), c(tpt), c(rhs))
      case New(tpt)                                    =>
        node(ReflectionType.New, c(tpt))
      case NamedArg(name, arg)                         =>
        node(ReflectionType.NamedArg, c(name), c(arg))
      case Select(qualifier, name)                     =>
        node(ReflectionType.Select, c(qualifier), c(name))
      case Match(selector, cases)                      =>
        node(ReflectionType.Match, c(selector), c(cases))
      case Closure(meth, tpt)                          =>
        node(ReflectionType.Closure, c(meth), c(tpt))
      case Block(stats, expr)                          =>
        node(ReflectionType.Block, c(stats), c(expr))
      case TypeBlock(aliases, tpt)                     =>
        node(ReflectionType.TypeBlock, c(aliases), c(tpt))
      case Assign(lhs, rhs)                            =>
        node(ReflectionType.Assign, c(lhs), c(rhs))
      case Ident(name)                                 =>
        node(ReflectionType.Ident, c(name))
      case _                                           => Ast.Leaf(path, "UnknownTree: " + tree.show)

    val p        = path
    val withSpan = ast.withSpan(Span(tree.pos.start, tree.pos.end))
    val withTpe  = tree match
      case tt: TypeTree =>
        withSpan.withMethodData(MethodKey.DotTpe, c(tt.tpe)(using summon)(using p / MethodKey.DotTpe))
      case _            => withSpan

    if tree.symbol.isNoSymbol then withTpe
    else
      withTpe
        .withMethodData(MethodKey.DotSymbol, c(tree.symbol)(using summon)(using p / MethodKey.DotSymbol))
        .withMethodData(
          MethodKey.DotSymbolDotFlags,
          getFlags(tree.symbol.flags)
        )

  private def getFlags(flags: Flags): List[Flag] =
    Flag.values.toList.filter(f => flags.is(f.toCompilerFlags))
