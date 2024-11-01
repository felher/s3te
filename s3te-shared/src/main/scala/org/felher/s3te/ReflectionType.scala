package org.felher.s3te

enum ReflectionType derives CanEqual, JsonEncoder:
  case OrType
  case TypeIdent
  case ShortConstant
  case AndType
  case FlexibleType
  case Flags
  case Term
  case FloatConstant
  case Return
  case LongConstant
  case NoPrefix
  case StringConstant
  case ByNameType
  case TypeBlock
  case TypeProjection
  case ParamRef
  case MethodOrPoly
  case ClassOfConstant
  case Constant
  case Repeated
  case ThisType
  case Typed
  case Annotated
  case This
  case MatchType
  case TypeLambda
  case Singleton
  case SourceFile
  case PackageClause
  case SimpleSelector
  case LambdaType
  case NullConstant
  case Inlined
  case CaseDef
  case RecursiveType
  case DefDef
  case ImplicitSearchFailure
  case PolyType
  case ClassDef
  case MethodType
  case Position
  case Export
  case WildcardTypeTree
  case TypedOrTest
  case TypeApply
  case Tree
  case ByName
  case Literal
  case CharConstant
  case OmitSelector
  case Inferred
  case LambdaTypeTree
  case Alternatives
  case NamedType
  case TypeBind
  case SelectOuter
  case TypeDef
  case Apply
  case Applied
  case TypeBoundsTree
  case Signature
  case Selector
  case TypeParamClause
  case AndOrType
  case Definition
  case ImplicitSearchSuccess
  case RecursiveThis
  case Try
  case ValOrDefDef
  case IntConstant
  case AppliedType
  case MatchTypeTree
  case BooleanConstant
  case Statement
  case Wildcard
  case While
  case TypeBounds
  case If
  case SuperType
  case Refinement
  case Symbol
  case Import
  case Refined
  case RenameSelector
  case Unapply
  case AmbiguousImplicits
  case Super
  case DivergingImplicit
  case ImplicitSearchResult
  case ByteConstant
  case NoMatchingImplicits
  case DoubleConstant
  case TypeRepr
  case SummonFrom
  case Bind
  case ValDef
  case AnnotatedType
  case TypeTree
  case New
  case NamedArg
  case Select
  case TermParamClause
  case ParamClause
  case Match
  case TermRef
  case Closure
  case Block
  case ConstantType
  case TypeRef
  case Assign
  case Ident
  case TypeSelect
  case GivenSelector
  case Ref
  case TypeCaseDef
  case MatchCase
  case UnitConstant

object ReflectionType:
  def parents(reflectionType: ReflectionType): List[ReflectionType] =
    reflectionType match
      case ReflectionType.OrType                => List(AndOrType)
      case ReflectionType.TypeIdent             => List(TypeTree)
      case ReflectionType.ShortConstant         => List(Constant)
      case ReflectionType.AndType               => List(AndOrType)
      case ReflectionType.FlexibleType          => List(TypeRepr)
      case ReflectionType.Flags                 => Nil
      case ReflectionType.Term                  => List(Statement)
      case ReflectionType.FloatConstant         => List(Constant)
      case ReflectionType.Return                => List(Term)
      case ReflectionType.LongConstant          => List(Constant)
      case ReflectionType.NoPrefix              => List(TypeRepr)
      case ReflectionType.StringConstant        => List(Constant)
      case ReflectionType.ByNameType            => List(TypeRepr)
      case ReflectionType.TypeBlock             => List(TypeTree)
      case ReflectionType.TypeProjection        => List(TypeTree)
      case ReflectionType.ParamRef              => List(TypeRepr)
      case ReflectionType.MethodOrPoly          => List(LambdaType)
      case ReflectionType.ClassOfConstant       => List(Constant)
      case ReflectionType.Constant              => Nil
      case ReflectionType.Repeated              => List(Term)
      case ReflectionType.ThisType              => List(TypeRepr)
      case ReflectionType.Typed                 => List(Term, TypedOrTest)
      case ReflectionType.Annotated             => List(TypeTree)
      case ReflectionType.This                  => List(Term)
      case ReflectionType.MatchType             => List(TypeRepr)
      case ReflectionType.TypeLambda            => List(LambdaType)
      case ReflectionType.Singleton             => List(TypeTree)
      case ReflectionType.SourceFile            => Nil
      case ReflectionType.PackageClause         => List(Tree)
      case ReflectionType.SimpleSelector        => List(Selector)
      case ReflectionType.LambdaType            => List(TypeRepr)
      case ReflectionType.NullConstant          => List(Constant)
      case ReflectionType.Inlined               => List(Term)
      case ReflectionType.CaseDef               => List(Tree)
      case ReflectionType.RecursiveType         => List(TypeRepr)
      case ReflectionType.DefDef                => List(ValOrDefDef)
      case ReflectionType.ImplicitSearchFailure => List(ImplicitSearchResult)
      case ReflectionType.PolyType              => List(MethodOrPoly)
      case ReflectionType.ClassDef              => List(Definition)
      case ReflectionType.MethodType            => List(MethodOrPoly)
      case ReflectionType.Position              => Nil
      case ReflectionType.Export                => List(Statement)
      case ReflectionType.WildcardTypeTree      => List(Tree)
      case ReflectionType.TypedOrTest           => List(Tree)
      case ReflectionType.TypeApply             => List(Term)
      case ReflectionType.Tree                  => Nil
      case ReflectionType.ByName                => List(TypeTree)
      case ReflectionType.Literal               => List(Term)
      case ReflectionType.CharConstant          => List(Constant)
      case ReflectionType.OmitSelector          => List(Selector)
      case ReflectionType.Inferred              => List(TypeTree)
      case ReflectionType.LambdaTypeTree        => List(TypeTree)
      case ReflectionType.Alternatives          => List(Tree)
      case ReflectionType.NamedType             => List(TypeRepr)
      case ReflectionType.TypeBind              => List(TypeTree)
      case ReflectionType.SelectOuter           => List(Term)
      case ReflectionType.TypeDef               => List(Definition)
      case ReflectionType.Apply                 => List(Term)
      case ReflectionType.Applied               => List(TypeTree)
      case ReflectionType.TypeBoundsTree        => List(Tree)
      case ReflectionType.Signature             => Nil
      case ReflectionType.Selector              => Nil
      case ReflectionType.TypeParamClause       => List(ParamClause)
      case ReflectionType.AndOrType             => List(TypeRepr)
      case ReflectionType.Definition            => List(Statement)
      case ReflectionType.ImplicitSearchSuccess => List(ImplicitSearchResult)
      case ReflectionType.RecursiveThis         => List(TypeRepr)
      case ReflectionType.Try                   => List(Term)
      case ReflectionType.ValOrDefDef           => List(Definition)
      case ReflectionType.IntConstant           => List(Constant)
      case ReflectionType.AppliedType           => List(TypeRepr)
      case ReflectionType.MatchTypeTree         => List(TypeTree)
      case ReflectionType.BooleanConstant       => List(Constant)
      case ReflectionType.Statement             => List(Tree)
      case ReflectionType.Wildcard              => List(Ident)
      case ReflectionType.While                 => List(Term)
      case ReflectionType.TypeBounds            => List(TypeRepr)
      case ReflectionType.If                    => List(Term)
      case ReflectionType.SuperType             => List(TypeRepr)
      case ReflectionType.Refinement            => List(TypeRepr)
      case ReflectionType.Symbol                => Nil
      case ReflectionType.Import                => List(Statement)
      case ReflectionType.Refined               => List(TypeTree)
      case ReflectionType.RenameSelector        => List(Selector)
      case ReflectionType.Unapply               => List(Tree)
      case ReflectionType.AmbiguousImplicits    => List(ImplicitSearchFailure)
      case ReflectionType.Super                 => List(Term)
      case ReflectionType.DivergingImplicit     => List(ImplicitSearchFailure)
      case ReflectionType.ImplicitSearchResult  => Nil
      case ReflectionType.ByteConstant          => List(Constant)
      case ReflectionType.NoMatchingImplicits   => List(ImplicitSearchFailure)
      case ReflectionType.DoubleConstant        => List(Constant)
      case ReflectionType.TypeRepr              => Nil
      case ReflectionType.SummonFrom            => List(Term)
      case ReflectionType.Bind                  => List(Tree)
      case ReflectionType.ValDef                => List(ValOrDefDef)
      case ReflectionType.AnnotatedType         => List(TypeRepr)
      case ReflectionType.TypeTree              => List(Tree)
      case ReflectionType.New                   => List(Term)
      case ReflectionType.NamedArg              => List(Term)
      case ReflectionType.Select                => List(Ref)
      case ReflectionType.TermParamClause       => List(ParamClause)
      case ReflectionType.ParamClause           => Nil
      case ReflectionType.Match                 => List(Term)
      case ReflectionType.TermRef               => List(NamedType)
      case ReflectionType.Closure               => List(Term)
      case ReflectionType.Block                 => List(Term)
      case ReflectionType.ConstantType          => List(TypeRepr)
      case ReflectionType.TypeRef               => List(NamedType)
      case ReflectionType.Assign                => List(Term)
      case ReflectionType.Ident                 => List(Ref)
      case ReflectionType.TypeSelect            => List(TypeTree)
      case ReflectionType.GivenSelector         => List(Selector)
      case ReflectionType.Ref                   => List(Term)
      case ReflectionType.TypeCaseDef           => List(Tree)
      case ReflectionType.MatchCase             => List(TypeRepr)
      case ReflectionType.UnitConstant          => List(Constant)

  def extractorNames(reflectionType: ReflectionType): Option[List[String]] =
    reflectionType match
      case ReflectionType.OrType                => Some(List("left", "right"))
      case ReflectionType.TypeIdent             => Some(List("name"))
      case ReflectionType.ShortConstant         => Some(List("value"))
      case ReflectionType.AndType               => Some(List("left", "right"))
      case ReflectionType.FlexibleType          => Some(List("tp"))
      case ReflectionType.Flags                 => None
      case ReflectionType.Term                  => None
      case ReflectionType.FloatConstant         => Some(List("value"))
      case ReflectionType.Return                => Some(List("expr", "from"))
      case ReflectionType.LongConstant          => Some(List("value"))
      case ReflectionType.NoPrefix              => Some(List())
      case ReflectionType.StringConstant        => Some(List("value"))
      case ReflectionType.ByNameType            => Some(List("underlying"))
      case ReflectionType.TypeBlock             => Some(List("aliases", "tpt"))
      case ReflectionType.TypeProjection        => Some(List("qualifier", "name"))
      case ReflectionType.ParamRef              => Some(List("binder", "idx"))
      case ReflectionType.MethodOrPoly          => None
      case ReflectionType.ClassOfConstant       => Some(List("value"))
      case ReflectionType.Constant              => None
      case ReflectionType.Repeated              => Some(List("elems", "elemtpt"))
      case ReflectionType.ThisType              => Some(List("tp"))
      case ReflectionType.Typed                 => Some(List("expr", "tpt"))
      case ReflectionType.Annotated             => Some(List("arg", "annot"))
      case ReflectionType.This                  => Some(List("qual"))
      case ReflectionType.MatchType             => Some(List("bound", "scrutinee", "cases"))
      case ReflectionType.TypeLambda            => Some(List("argNames", "argBounds", "resType"))
      case ReflectionType.Singleton             => Some(List("ref"))
      case ReflectionType.SourceFile            => None
      case ReflectionType.PackageClause         => Some(List("pid", "stats"))
      case ReflectionType.SimpleSelector        => Some(List("id"))
      case ReflectionType.LambdaType            => None
      case ReflectionType.NullConstant          => Some(List())
      case ReflectionType.Inlined               => Some(List("call", "bindings", "expansion"))
      case ReflectionType.CaseDef               => Some(List("pat", "guard", "body"))
      case ReflectionType.RecursiveType         => Some(List("underlying"))
      case ReflectionType.DefDef                => Some(List("name", "paramsClauses", "returnTpt", "rhs"))
      case ReflectionType.ImplicitSearchFailure => None
      case ReflectionType.PolyType              => Some(List("argNames", "argBounds", "resType"))
      case ReflectionType.ClassDef              => Some(List("name", "constr", "parents", "self", "body"))
      case ReflectionType.MethodType            => Some(List("argNames", "argTypes", "resType"))
      case ReflectionType.Position              => None
      case ReflectionType.Export                => Some(List("expr", "selectors"))
      case ReflectionType.WildcardTypeTree      => Some(List())
      case ReflectionType.TypedOrTest           => Some(List("tree", "tpt"))
      case ReflectionType.TypeApply             => Some(List("fun", "args"))
      case ReflectionType.Tree                  => None
      case ReflectionType.ByName                => Some(List("result"))
      case ReflectionType.Literal               => Some(List("const"))
      case ReflectionType.CharConstant          => Some(List("value"))
      case ReflectionType.OmitSelector          => Some(List("id"))
      case ReflectionType.Inferred              => Some(List())
      case ReflectionType.LambdaTypeTree        => Some(List("tparams", "body"))
      case ReflectionType.Alternatives          => Some(List("patterns"))
      case ReflectionType.NamedType             => None
      case ReflectionType.TypeBind              => Some(List("name", "bounds"))
      case ReflectionType.SelectOuter           => None
      case ReflectionType.TypeDef               => Some(List("name", "rhs"))
      case ReflectionType.Apply                 => Some(List("fun", "args"))
      case ReflectionType.Applied               => Some(List("tpt", "args"))
      case ReflectionType.TypeBoundsTree        => Some(List("lo", "hi"))
      case ReflectionType.Signature             => None
      case ReflectionType.Selector              => None
      case ReflectionType.TypeParamClause       => Some(List("params"))
      case ReflectionType.AndOrType             => None
      case ReflectionType.Definition            => None
      case ReflectionType.ImplicitSearchSuccess => None
      case ReflectionType.RecursiveThis         => Some(List("binder"))
      case ReflectionType.Try                   => Some(List("block", "handlers", "finalizer"))
      case ReflectionType.ValOrDefDef           => None
      case ReflectionType.IntConstant           => Some(List("value"))
      case ReflectionType.AppliedType           => Some(List("tycon", "args"))
      case ReflectionType.MatchTypeTree         => Some(List("bound", "selector", "cases"))
      case ReflectionType.BooleanConstant       => Some(List("value"))
      case ReflectionType.Statement             => None
      case ReflectionType.Wildcard              => Some(List())
      case ReflectionType.While                 => Some(List("cond", "body"))
      case ReflectionType.TypeBounds            => Some(List("lo", "hi"))
      case ReflectionType.If                    => Some(List("cond", "thenp", "elsep"))
      case ReflectionType.SuperType             => Some(List("thistpe", "supertpe"))
      case ReflectionType.Refinement            => Some(List("parent", "name", "info"))
      case ReflectionType.Symbol                => None
      case ReflectionType.Import                => Some(List("expr", "selectors"))
      case ReflectionType.Refined               => Some(List("tpt", "refinements"))
      case ReflectionType.RenameSelector        => Some(List("id1", "id2"))
      case ReflectionType.Unapply               => Some(List("fun", "implicits", "patterns"))
      case ReflectionType.AmbiguousImplicits    => None
      case ReflectionType.Super                 => Some(List("qual", "mix"))
      case ReflectionType.DivergingImplicit     => None
      case ReflectionType.ImplicitSearchResult  => None
      case ReflectionType.ByteConstant          => Some(List("value"))
      case ReflectionType.NoMatchingImplicits   => None
      case ReflectionType.DoubleConstant        => Some(List("value"))
      case ReflectionType.TypeRepr              => None
      case ReflectionType.SummonFrom            => Some(List("cases"))
      case ReflectionType.Bind                  => Some(List("name", "body"))
      case ReflectionType.ValDef                => Some(List("name", "tpt", "rhs"))
      case ReflectionType.AnnotatedType         => Some(List("underlying", "annot"))
      case ReflectionType.TypeTree              => None
      case ReflectionType.New                   => Some(List("tpt"))
      case ReflectionType.NamedArg              => Some(List("name", "arg"))
      case ReflectionType.Select                => Some(List("qualifier", "name"))
      case ReflectionType.TermParamClause       => Some(List("params"))
      case ReflectionType.ParamClause           => None
      case ReflectionType.Match                 => Some(List("selector", "cases"))
      case ReflectionType.TermRef               => Some(List("qual", "name"))
      case ReflectionType.Closure               => Some(List("meth", "tpt"))
      case ReflectionType.Block                 => Some(List("stats", "expr"))
      case ReflectionType.ConstantType          => Some(List("value"))
      case ReflectionType.TypeRef               => Some(List("qual", "name"))
      case ReflectionType.Assign                => Some(List("lhs", "rhs"))
      case ReflectionType.Ident                 => Some(List("name"))
      case ReflectionType.TypeSelect            => Some(List("qualifier", "name"))
      case ReflectionType.GivenSelector         => Some(List("bound"))
      case ReflectionType.Ref                   => None
      case ReflectionType.TypeCaseDef           => Some(List("pat", "body"))
      case ReflectionType.MatchCase             => Some(List("pat", "rhs"))
      case ReflectionType.UnitConstant          => Some(List())

  def scalaDoc(tpe: ReflectionType): String = tpe match
    case Alternatives          => """
                  <p>Pattern representing <code>X | Y | ...</code> alternatives.</p>
                 """
    case AmbiguousImplicits    => """"""
    case AndOrType             => """
                  <p>Intersection type <code>T &amp; U</code> or an union type <code>T | U</code></p>
                 """
    case AndType               => """
                  <p>Intersection type <code>T &amp; U</code></p>
                 """
    case Annotated             => """
                  <p>Type tree representing an annotated type</p>
                 """
    case AnnotatedType         => """
                  <p>A type with an annotation <code>T @foo</code></p>
                 """
    case Applied               => """
                  <p>Type tree representing a type application</p>
                 """
    case AppliedType           => """
                  <p>A higher kinded type applied to some types <code>T[U]</code></p>
                 """
    case Apply                 =>
      """
                  <p>Tree representing an application of arguments. It represents a single list of arguments, multiple argument lists will have nested <code>Apply</code>s</p>
                 """
    case Assign                => """
                  <p>Tree representing an assignment <code>x = y</code> in the source code</p>
                 """
    case Bind                  => """
                  <p>Pattern representing a <code>_ @ _</code> binding.</p>
                 """
    case Block                 => """
                  <p>Tree representing a block <code>{ ... }</code> in the source code</p>
                 """
    case BooleanConstant       => """
                  <p>Constant Boolean value</p>
                 """
    case ByName                => """
                  <p>Type tree representing a by name parameter</p>
                 """
    case ByNameType            =>
      """
                  <p>Type of a by-name definition of type <code>=&gt;T</code>.</p>
                  <p>May represent by-name parameter such as <code>thunk</code> in</p>
                  <div class="snippet mono-small-block" scala-snippet="" runnable="">
                   <pre><code class="language-scala hljs"><span line-number="1" class="hideable hidden"><span class="tooltip-container"></span><span class="hljs-typedef"><span class="hljs-keyword">type</span> <span class="hljs-type">T</span></span>
</span><span line-number="2" class=""><span class="tooltip-container"></span><span class="hljs-function"><span class="hljs-keyword">def</span> <span class="hljs-title">log</span>[<span class="hljs-type">T</span>](<span class="hljs-params">thunk: <span class="hljs-keyword">=&gt;</span> <span class="hljs-type">T</span></span>): <span class="hljs-type">T</span></span> = <span class="hljs-built_in">???</span>
</span></code></pre>
                  </div>
                  <p>May also represent a the return type of a parameterless method definition such as</p>
                  <div class="snippet mono-small-block" scala-snippet="" runnable="">
                 <pre><code class="language-scala hljs"><span line-number="1" class=""><span class="tooltip-container"></span><span class="hideable hidden">  </span><span class="hljs-function"><span class="hljs-keyword">def</span> <span class="hljs-title">foo</span>: <span class="hljs-type">Int</span></span> = <span class="hljs-built_in">???</span>
</span></code></pre>
                  </div>
                 """
    case ByteConstant          => """
                  <p>Constant Byte value</p>
                 """
    case CaseDef               => """
                  <p>Branch of a pattern match or catch clause</p>
                 """
    case CharConstant          => """
                  <p>Constant Char value</p>
                 """
    case ClassDef              =>
      """
                  <p>Tree representing a class definition. This includes anonymous class definitions and the class of a module object</p>
                 """
    case ClassOfConstant       => """
                  <p>Constant class value representing a <code>classOf[T]</code></p>
                 """
    case Closure               =>
      """
                  <p>A lambda <code>(...) =&gt; ...</code> in the source code is represented as a local method and a closure:</p>
                  <p>{ def m(...) = ... closure(m) }</p>
                 """
    case Constant              => """
                  <p>Constant value represented as the constant itself</p>
                 """
    case ConstantType          => """
                  <p>A singleton type representing a known constant value</p>
                 """
    case DefDef                => """
                  <p>Tree representing a method definition in the source code</p>
                 """
    case Definition            =>
      """
                  <p>Tree representing a definition in the source code. It can be <code>ClassDef</code>, <code>TypeDef</code>, <code>DefDef</code> or <code>ValDef</code></p>
                 """
    case DivergingImplicit     => """"""
    case DoubleConstant        => """
                  <p>Constant Double value</p>
                 """
    case Export                =>
      """
                  <p>Tree representing an export clause in the source code. Export forwarders generated from this clause appear in the same scope.</p>
                 """
    case FlexibleType          => """
                  <p>Flexible types for explicit nulls</p>
                 """
    case Flags                 => """
                  <p>Flags of a Symbol</p>
                 """
    case FloatConstant         => """
                  <p>Constant Float value</p>
                 """
    case GivenSelector         =>
      """
                  <p>given import/export selector: <code>.given</code>/<code>.{given T}</code> in <code>import foo.given</code>/<code>export foo.{given T}</code></p>
                 """
    case Ident                 => """
                  <p>Tree representing a reference to definition with a given name</p>
                 """
    case If                    => """
                  <p>Tree representing an if/then/else <code>if (...) ... else ...</code> in the source code</p>
                 """
    case ImplicitSearchFailure => """"""
    case ImplicitSearchResult  => """
                  <p>Result of a given instance search</p>
                 """
    case ImplicitSearchSuccess => """"""
    case Import                => """
                  <p>Tree representing an import in the source code.</p>
                  <p>See also documentation on <code>Selector</code>.</p>
                 """
    case Inferred              => """
                  <p>Type tree representing an inferred type</p>
                 """
    case Inlined               => """
                  <p>Tree representing the scope of an inlined tree</p>
                 """
    case IntConstant           => """
                  <p>Constant Int value</p>
                 """
    case LambdaType            => """
                  <p>Type of the definition of a method taking a single list of type or term parameters</p>
                 """
    case LambdaTypeTree        => """
                  <p>Type tree representing a lambda abstraction type</p>
                 """
    case Literal               => """
                  <p>Tree representing a literal value in the source code</p>
                 """
    case LongConstant          => """
                  <p>Constant Long value</p>
                 """
    case Match                 => """
                  <p>Tree representing a pattern match <code>x match { ... }</code> in the source code</p>
                 """
    case MatchCase             => """
                  <p>Case of a <code>MatchType</code> containing pattern <code>case P =&gt; R</code>.</p>
                  <p>Note: cases with type bindings are represented nested in a <code>TypeLambda</code>.</p>
                 """
    case MatchType             => """
                  <p>Type match <code>T match { case U =&gt; ... }</code></p>
                 """
    case MatchTypeTree         => """
                  <p>Type tree representing a type match</p>
                 """
    case MethodOrPoly          => """
                  <p>Type of the definition of a method taking a single list of type or term parameters</p>
                 """
    case MethodType            =>
      """
                  <p>Type of the definition of a method taking a single list of parameters. It's return type may be a MethodType.</p>
                 """
    case NamedArg              =>
      """
                  <p>Tree representing an argument passed with an explicit name. Such as <code>arg1 = x</code> in <code>foo(arg1 = x)</code></p>
                 """
    case NamedType             => """
                  <p>Type of a reference to a type or term symbol</p>
                 """
    case New                   => """
                  <p>Tree representing <code>new</code> in the source code</p>
                 """
    case NoMatchingImplicits   => """"""
    case NoPrefix              => """
                  <p>NoPrefix for a type selection</p>
                 """
    case NullConstant          => """
                  <p>Constant null value</p>
                 """
    case OmitSelector          =>
      """
                  <p>Omit import/export selector: <code>.{bar =&gt; _}</code> in <code>import foo.{bar =&gt; _}</code></p>
                 """
    case OrType                => """
                  <p>Union type <code>T | U</code></p>
                 """
    case PackageClause         =>
      """
                  <p>Tree representing a package clause in the source code</p>
                  <div class="snippet mono-small-block" scala-snippet="">
                   <pre><code class="language-scala hljs"><span line-number="1" class=""><span class="tooltip-container"></span><span class="hljs-package"><span class="hljs-keyword">package</span> <span class="hljs-title">foo</span> </span>{
    </span><span line-number="2" class=""><span class="tooltip-container"></span>  <span class="hljs-comment">// package stats</span>
    </span><span line-number="3" class=""><span class="tooltip-container"></span>}
    </span></code></pre>
                  </div>
                  <p>or</p>
                  <div class="snippet mono-small-block" scala-snippet="">
                   <pre><code class="language-scala hljs"><span line-number="1" class=""><span class="tooltip-container"></span><span class="hljs-keyword">package</span> foo.bar
    </span><span line-number="2" class=""><span class="tooltip-container"></span><span class="hljs-comment">// package stats</span>
    </span></code></pre>
                  </div>
                 """
    case ParamClause           =>
      """
                  <p>A parameter clause <code>[X1, ..., Xn]</code> or <code>(x1: X1, ..., xn: Xx)</code></p>
                  <p><code>[X1, ..., Xn]</code> are represented with <code>TypeParamClause</code> and <code>(x1: X1, ..., xn: Xx)</code> are represented with <code>TermParamClause</code></p>
                  <p><code>ParamClause</code> encodes the following enumeration</p>
                  <div class="snippet mono-small-block" scala-snippet="" runnable="">
                   <pre><code class="language-scala hljs"><span line-number="1" class="hideable hidden"><span class="tooltip-container"></span><span class="hljs-keyword">import</span> scala.quoted.*
    </span><span line-number="2" class="hideable hidden"><span class="tooltip-container"></span><span class="hljs-function"><span class="hljs-keyword">def</span> <span class="hljs-title">inQuotes</span><span class="hljs-params">(<span class="hljs-keyword">using</span> <span class="hljs-type">Quotes</span></span>)</span> = {
    </span><span line-number="3" class="hideable hidden"><span class="tooltip-container"></span>  <span class="hljs-keyword">val</span> <span class="hljs-title">q</span>: <span class="hljs-type">Quotes</span> = <span class="hljs-built_in">summon</span>[<span class="hljs-type">Quotes</span>]
    </span><span line-number="4" class="hideable hidden"><span class="tooltip-container"></span>  <span class="hljs-keyword">import</span> q.reflect.*
    </span><span line-number="5" class=""><span class="tooltip-container"></span><span class="hideable hidden">  </span><span class="hljs-class"><span class="hljs-keyword">enum</span> <span class="hljs-title">ParamClause</span></span>:
    </span><span line-number="6" class=""><span class="tooltip-container"></span><span class="hideable hidden">  </span>  <span class="hljs-keyword">case</span> <span class="hljs-title">TypeParamClause</span>(<span class="hljs-params">params: <span class="hljs-type">List</span>[<span class="hljs-type">TypeDef</span>]</span>)
    </span><span line-number="7" class=""><span class="tooltip-container"></span><span class="hideable hidden">  </span>  <span class="hljs-keyword">case</span> <span class="hljs-title">TermParamClause</span>(<span class="hljs-params">params: <span class="hljs-type">List</span>[<span class="hljs-type">ValDef</span>]</span>)
    </span><span line-number="8" class="hideable hidden"><span class="tooltip-container"></span>}
    </span></code></pre>
                  </div>
                 """
    case ParamRef              => """
                  <p>Type of a parameter reference</p>
                 """
    case PolyType              =>
      """
                  <p>Type of the definition of a method taking a list of type parameters. It's return type may be a MethodType.</p>
                 """
    case Position              => """
                  <p>Position in a source file</p>
                 """
    case RecursiveThis         => """
                  <p>A type that is recursively defined <code>this</code></p>
                 """
    case RecursiveType         => """
                  <p>A type that is recursively defined</p>
                 """
    case Ref                   => """
                  <p>Tree representing a reference to definition</p>
                 """
    case Refined               => """
                  <p>Type tree representing a type refinement</p>
                 """
    case Refinement            => """
                  <p>A type with a type refinement <code>T { type U }</code></p>
                 """
    case RenameSelector        =>
      """
                  <p>Rename import/export selector: <code>.{bar =&gt; baz}</code> in <code>import foo.{bar =&gt; baz}</code></p>
                 """
    case Repeated              =>
      """
                  <p>Tree representing a variable argument list in the source code.</p>
                  <p>This tree is used to encode varargs terms. The Repeated encapsulates the sequence of the elements but needs to be wrapped in a <code>scala.&lt;repeated&gt;[T]</code> (see <code>defn.RepeatedParamClass</code>). For example the arguments <code>1, 2</code> of <code>List.apply(1, 2)</code> can be represented as follows:</p>
                  <div class="snippet mono-small-block" scala-snippet="" runnable="">
                   <pre><code class="language-scala hljs"><span line-number="1" class="hideable hidden"><span class="tooltip-container"></span><span class="hljs-keyword">import</span> scala.quoted.<span class="hljs-keyword">_</span>
    </span><span line-number="2" class="hideable hidden"><span class="tooltip-container"></span><span class="hljs-function"><span class="hljs-keyword">def</span> <span class="hljs-title">inQuotes</span><span class="hljs-params">(<span class="hljs-keyword">using</span> <span class="hljs-type">Quotes</span></span>)</span> = {
    </span><span line-number="3" class="hideable hidden"><span class="tooltip-container"></span>  <span class="hljs-keyword">val</span> <span class="hljs-title">q</span>: <span class="hljs-type">Quotes</span> = <span class="hljs-built_in">summon</span>[<span class="hljs-type">Quotes</span>]
    </span><span line-number="4" class="hideable hidden"><span class="tooltip-container"></span>  <span class="hljs-keyword">import</span> q.reflect.<span class="hljs-keyword">_</span>
    </span><span line-number="5" class=""><span class="tooltip-container"></span><span class="hideable hidden">  </span><span class="hljs-keyword">val</span> <span class="hljs-title">intArgs </span>= <span class="hljs-type">List</span>(<span class="hljs-type">Literal</span>(<span class="hljs-type">IntConstant</span>(<span class="hljs-number">1</span>)), <span class="hljs-type">Literal</span>(<span class="hljs-type">IntConstant</span>(<span class="hljs-number">2</span>)))
    </span><span line-number="6" class=""><span class="tooltip-container"></span><span class="hideable hidden">  </span><span class="hljs-type">Typed</span>(
    </span><span line-number="7" class=""><span class="tooltip-container"></span><span class="hideable hidden">  </span>  <span class="hljs-type">Repeated</span>(intArgs, <span class="hljs-type">TypeTree</span>.of[<span class="hljs-type">Int</span>]),
    </span><span line-number="8" class=""><span class="tooltip-container"></span><span class="hideable hidden">  </span>  <span class="hljs-type">Inferred</span>(defn.<span class="hljs-type">RepeatedParamClass</span>.typeRef.appliedTo(<span class="hljs-type">TypeRepr</span>.of[<span class="hljs-type">Int</span>]))
    </span><span line-number="9" class=""><span class="tooltip-container"></span><span class="hideable hidden">  </span>)
    </span><span line-number="10" class="hideable hidden"><span class="tooltip-container"></span>}
    </span></code></pre>
                  </div>
                 """
    case Return                => """
                  <p>Tree representing a <code>return</code> in the source code</p>
                 """
    case Select                => """
                  <p>Tree representing a selection of definition with a given name on a given prefix</p>
                 """
    case SelectOuter           =>
      """
                  <p>Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees</p>
                 """
    case Selector              =>
      """
                  <p>Import/Export selectors:</p>
                  <ul>
                   <li>SimpleSelector: <code>.bar</code> in <code>import foo.bar</code></li>
                   <li>RenameSelector: <code>.{bar =&gt; baz}</code> in <code>export foo.{bar =&gt; baz}</code></li>
                   <li>OmitSelector: <code>.{bar =&gt; _}</code> in <code>import foo.{bar =&gt; _}</code></li>
                   <li>GivenSelector: <code>.given</code>/<code>.{given T}</code> in <code>export foo.given</code>/<code>import foo.{given T}</code></li>
                  </ul>
                 """
    case ShortConstant         => """
                  <p>Constant Short value</p>
                 """
    case Signature             => """
                  <p>The signature of a method</p>
                 """
    case SimpleSelector        => """
                  <p>Simple import/export selector: <code>.bar</code> in <code>import foo.bar</code></p>
                 """
    case Singleton             => """
                  <p>Type tree representing a singleton type</p>
                 """
    case SourceFile            => """
                  <p>Scala source file</p>
                 """
    case Statement             => """
                  <p>Tree representing a statement in the source code</p>
                 """
    case StringConstant        => """
                  <p>Constant String value</p>
                 """
    case SummonFrom            => """
                  <p>Tree representing a summoning match <code>summonFrom { ... }</code> in the source code</p>
                 """
    case Super                 => """
                  <p>Tree representing <code>super</code> in the source code</p>
                 """
    case SuperType             => """
                  <p>Type of a <code>super</code> reference</p>
                 """
    case Symbol                =>
      """
                  <p>Symbol of a definition. Symbols can be compared with <code>==</code> to know if two definitions are the same.</p>
                 """
    case Term                  => """
                  <p>Tree representing an expression in the source code</p>
                 """
    case TermParamClause       =>
      """
                  <p>A term parameter clause <code>(x1: X1, ..., xn: Xx)</code> Can also be <code>(implicit X1, ..., Xn)</code>, <code>(given X1, ..., Xn)</code> or <code>(given x1: X1, ..., xn: Xn)</code></p>
                 """
    case TermRef               => """
                  <p>Type of a reference to a term symbol</p>
                 """
    case This                  => """
                  <p>Tree representing <code>this</code> or <code>C.this</code> in the source code</p>
                 """
    case ThisType              => """
                  <p>Type of <code>this</code></p>
                 """
    case Tree                  => """
                  <p>Tree representing code written in the source</p>
                 """
    case Try                   =>
      """
                  <p>Tree representing a try catch <code>try x catch { ... } finally { ... }</code> in the source code</p>
                 """
    case TypeApply             => """
                  <p>Tree representing an application of type arguments</p>
                 """
    case TypeBind              => """
                  <p>Type tree representing a type binding</p>
                 """
    case TypeBlock             => """
                  <p>Type tree within a block with aliases <code>{ type U1 = ... ; T[U1, U2] }</code></p>
                 """
    case TypeBounds            => """
                  <p>Type bounds</p>
                 """
    case TypeBoundsTree        => """
                  <p>Type tree representing a type bound written in the source</p>
                 """
    case TypeCaseDef           => """
                  <p>Branch of a type pattern match</p>
                 """
    case TypeDef               => """
                  <p>Tree representing a type (parameter or member) definition in the source code</p>
                 """
    case TypeIdent             => """
                  <p>Type tree representing a reference to definition with a given name</p>
                 """
    case TypeLambda            =>
      """
                  <p>Type of the definition of a type lambda taking a list of type parameters. It's return type may be a TypeLambda.</p>
                 """
    case TypeParamClause       => """
                  <p>A type parameter clause <code>[X1, ..., Xn]</code></p>
                 """
    case TypeProjection        => """
                  <p>Type tree representing a selection of definition with a given name on a given type prefix</p>
                 """
    case TypeRef               => """
                  <p>Type of a reference to a type symbol</p>
                 """
    case TypeRepr              => """
                  <p>A type, type constructors, type bounds or NoPrefix</p>
                 """
    case TypeSelect            => """
                  <p>Type tree representing a selection of definition with a given name on a given term prefix</p>
                 """
    case TypeTree              => """
                  <p>Type tree representing a type written in the source</p>
                 """
    case Typed                 =>
      """
                  <p>Tree representing a type ascription <code>x: T</code> in the source code.</p>
                  <p>Also represents a pattern that contains a term <code>x</code>. Other <code>: T</code> patterns use the more general <code>TypedOrTest</code>.</p>
                 """
    case TypedOrTest           => """
                  <p>Tree representing a type ascription or type test pattern <code>x: T</code> in the source code.</p>
                 """
    case Unapply               => """
                  <p>Pattern representing a <code>Xyz(...)</code> unapply.</p>
                 """
    case UnitConstant          => """
                  <p>Constant Unit value</p>
                 """
    case ValDef                =>
      """
                  <p>Tree representing a value definition in the source code. This includes <code>val</code>, <code>lazy val</code>, <code>var</code>, <code>object</code> and parameter definitions.</p>
                 """
    case ValOrDefDef           =>
      """
                  <p>Tree representing a value or method definition in the source code. This includes <code>def</code>, <code>val</code>, <code>lazy val</code>, <code>var</code>, <code>object</code> and parameter definitions.</p>
                 """
    case While                 => """
                  <p>Tree representing a while loop</p>
                 """
    case Wildcard              => """
                  <p>Pattern representing a <code>_</code> wildcard.</p>
                 """
    case WildcardTypeTree      =>
      """
                  <p>Type tree representing wildcard type bounds written in the source. The wildcard type <code>_</code> (for example in in <code>List[_]</code>) will be a type tree that represents a type but has <code>TypeBounds</code> inside.</p>
                 """

  def scalaDocShort(tpe: ReflectionType): String = tpe match
    case Alternatives          => """
                 <p>Pattern representing <code>X | Y | ...</code> alternatives.</p>
                """
    case AmbiguousImplicits    => """"""
    case AndOrType             => """
                 <p>Intersection type <code>T &amp; U</code> or an union type <code>T | U</code></p>
                """
    case AndType               => """
                 <p>Intersection type <code>T &amp; U</code></p>
                """
    case Annotated             => """
                 <p>Type tree representing an annotated type</p>
                """
    case AnnotatedType         => """
                 <p>A type with an annotation <code>T @foo</code></p>
                """
    case Applied               => """
                 <p>Type tree representing a type application</p>
                """
    case AppliedType           => """
                 <p>A higher kinded type applied to some types <code>T[U]</code></p>
                """
    case Apply                 =>
      """
                 <p>Tree representing an application of arguments. It represents a single list of arguments, multiple argument lists will have nested <code>Apply</code>s</p>
                """
    case Assign                => """
                 <p>Tree representing an assignment <code>x = y</code> in the source code</p>
                """
    case Bind                  => """
                 <p>Pattern representing a <code>_ @ _</code> binding.</p>
                """
    case Block                 => """
                 <p>Tree representing a block <code>{ ... }</code> in the source code</p>
                """
    case BooleanConstant       => """
                 <p>Constant Boolean value</p>
                """
    case ByName                => """
                 <p>Type tree representing a by name parameter</p>
                """
    case ByNameType            => """
                 <p>Type of a by-name definition of type <code>=&gt;T</code>.</p>
                """
    case ByteConstant          => """
                 <p>Constant Byte value</p>
                """
    case CaseDef               => """
                 <p>Branch of a pattern match or catch clause</p>
                """
    case CharConstant          => """
                 <p>Constant Char value</p>
                """
    case ClassDef              =>
      """
                 <p>Tree representing a class definition. This includes anonymous class definitions and the class of a module object</p>
                """
    case ClassOfConstant       => """
                 <p>Constant class value representing a <code>classOf[T]</code></p>
                """
    case Closure               =>
      """
                 <p>A lambda <code>(...) =&gt; ...</code> in the source code is represented as a local method and a closure:</p>
                """
    case Constant              => """
                 <p>Constant value represented as the constant itself</p>
                """
    case ConstantType          => """
                 <p>A singleton type representing a known constant value</p>
                """
    case DefDef                => """
                 <p>Tree representing a method definition in the source code</p>
                """
    case Definition            =>
      """
                 <p>Tree representing a definition in the source code. It can be <code>ClassDef</code>, <code>TypeDef</code>, <code>DefDef</code> or <code>ValDef</code></p>
                """
    case DivergingImplicit     => """"""
    case DoubleConstant        => """
                 <p>Constant Double value</p>
                """
    case Export                =>
      """
                 <p>Tree representing an export clause in the source code. Export forwarders generated from this clause appear in the same scope.</p>
                """
    case FlexibleType          => """
                 <p>Flexible types for explicit nulls</p>
                """
    case Flags                 => """
                  <p>Flags of a Symbol</p>
                 """
    case FloatConstant         => """
                 <p>Constant Float value</p>
                """
    case GivenSelector         =>
      """
                 <p>given import/export selector: <code>.given</code>/<code>.{given T}</code> in <code>import foo.given</code>/<code>export foo.{given T}</code></p>
                """
    case Ident                 => """
                 <p>Tree representing a reference to definition with a given name</p>
                """
    case If                    => """
                 <p>Tree representing an if/then/else <code>if (...) ... else ...</code> in the source code</p>
                """
    case ImplicitSearchFailure => """"""
    case ImplicitSearchResult  => """
                 <p>Result of a given instance search</p>
                """
    case ImplicitSearchSuccess => """"""
    case Import                => """
                 <p>Tree representing an import in the source code.</p>
                """
    case Inferred              => """
                 <p>Type tree representing an inferred type</p>
                """
    case Inlined               => """
                 <p>Tree representing the scope of an inlined tree</p>
                """
    case IntConstant           => """
                 <p>Constant Int value</p>
                """
    case LambdaType            => """
                 <p>Type of the definition of a method taking a single list of type or term parameters</p>
                """
    case LambdaTypeTree        => """
                 <p>Type tree representing a lambda abstraction type</p>
                """
    case Literal               => """
                 <p>Tree representing a literal value in the source code</p>
                """
    case LongConstant          => """
                 <p>Constant Long value</p>
                """
    case Match                 => """
                 <p>Tree representing a pattern match <code>x match { ... }</code> in the source code</p>
                """
    case MatchCase             => """
                 <p>Case of a <code>MatchType</code> containing pattern <code>case P =&gt; R</code>.</p>
                """
    case MatchType             => """
                 <p>Type match <code>T match { case U =&gt; ... }</code></p>
                """
    case MatchTypeTree         => """
                 <p>Type tree representing a type match</p>
                """
    case MethodOrPoly          => """
                 <p>Type of the definition of a method taking a single list of type or term parameters</p>
                """
    case MethodType            =>
      """
                 <p>Type of the definition of a method taking a single list of parameters. It's return type may be a MethodType.</p>
                """
    case NamedArg              =>
      """
                 <p>Tree representing an argument passed with an explicit name. Such as <code>arg1 = x</code> in <code>foo(arg1 = x)</code></p>
                """
    case NamedType             => """
                 <p>Type of a reference to a type or term symbol</p>
                """
    case New                   => """
                 <p>Tree representing <code>new</code> in the source code</p>
                """
    case NoMatchingImplicits   => """"""
    case NoPrefix              => """
                 <p>NoPrefix for a type selection</p>
                """
    case NullConstant          => """
                 <p>Constant null value</p>
                """
    case OmitSelector          =>
      """
                 <p>Omit import/export selector: <code>.{bar =&gt; _}</code> in <code>import foo.{bar =&gt; _}</code></p>
                """
    case OrType                => """
                 <p>Union type <code>T | U</code></p>
                """
    case PackageClause         => """
                 <p>Tree representing a package clause in the source code</p>
                """
    case ParamClause           => """
                 <p>A parameter clause <code>[X1, ..., Xn]</code> or <code>(x1: X1, ..., xn: Xx)</code></p>
                """
    case ParamRef              => """
                 <p>Type of a parameter reference</p>
                """
    case PolyType              =>
      """
                 <p>Type of the definition of a method taking a list of type parameters. It's return type may be a MethodType.</p>
                """
    case Position              => """
                 <p>Position in a source file</p>
                """
    case RecursiveThis         => """
                 <p>A type that is recursively defined <code>this</code></p>
                """
    case RecursiveType         => """
                 <p>A type that is recursively defined</p>
                """
    case Ref                   => """
                 <p>Tree representing a reference to definition</p>
                """
    case Refined               => """
                 <p>Type tree representing a type refinement</p>
                """
    case Refinement            => """
                 <p>A type with a type refinement <code>T { type U }</code></p>
                """
    case RenameSelector        =>
      """
                 <p>Rename import/export selector: <code>.{bar =&gt; baz}</code> in <code>import foo.{bar =&gt; baz}</code></p>
                """
    case Repeated              => """
                 <p>Tree representing a variable argument list in the source code.</p>
                """
    case Return                => """
                 <p>Tree representing a <code>return</code> in the source code</p>
                """
    case Select                => """
                 <p>Tree representing a selection of definition with a given name on a given prefix</p>
                """
    case SelectOuter           =>
      """
                 <p>Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees</p>
                """
    case Selector              => """
                 <p>Import/Export selectors:</p>
                """
    case ShortConstant         => """
                 <p>Constant Short value</p>
                """
    case Signature             => """
                 <p>The signature of a method</p>
                """
    case SimpleSelector        => """
                 <p>Simple import/export selector: <code>.bar</code> in <code>import foo.bar</code></p>
                """
    case Singleton             => """
                 <p>Type tree representing a singleton type</p>
                """
    case SourceFile            => """
                 <p>Scala source file</p>
                """
    case Statement             => """
                 <p>Tree representing a statement in the source code</p>
                """
    case StringConstant        => """
                 <p>Constant String value</p>
                """
    case SummonFrom            => """
                 <p>Tree representing a summoning match <code>summonFrom { ... }</code> in the source code</p>
                """
    case Super                 => """
                 <p>Tree representing <code>super</code> in the source code</p>
                """
    case SuperType             => """
                 <p>Type of a <code>super</code> reference</p>
                """
    case Symbol                =>
      """
                 <p>Symbol of a definition. Symbols can be compared with <code>==</code> to know if two definitions are the same.</p>
                """
    case Term                  => """
                 <p>Tree representing an expression in the source code</p>
                """
    case TermParamClause       =>
      """
                 <p>A term parameter clause <code>(x1: X1, ..., xn: Xx)</code> Can also be <code>(implicit X1, ..., Xn)</code>, <code>(given X1, ..., Xn)</code> or <code>(given x1: X1, ..., xn: Xn)</code></p>
                """
    case TermRef               => """
                 <p>Type of a reference to a term symbol</p>
                """
    case This                  => """
                 <p>Tree representing <code>this</code> or <code>C.this</code> in the source code</p>
                """
    case ThisType              => """
                 <p>Type of <code>this</code></p>
                """
    case Tree                  => """
                 <p>Tree representing code written in the source</p>
                """
    case Try                   =>
      """
                 <p>Tree representing a try catch <code>try x catch { ... } finally { ... }</code> in the source code</p>
                """
    case TypeApply             => """
                 <p>Tree representing an application of type arguments</p>
                """
    case TypeBind              => """
                 <p>Type tree representing a type binding</p>
                """
    case TypeBlock             => """
                 <p>Type tree within a block with aliases <code>{ type U1 = ... ; T[U1, U2] }</code></p>
                """
    case TypeBounds            => """
                 <p>Type bounds</p>
                """
    case TypeBoundsTree        => """
                 <p>Type tree representing a type bound written in the source</p>
                """
    case TypeCaseDef           => """
                 <p>Branch of a type pattern match</p>
                """
    case TypeDef               => """
                 <p>Tree representing a type (parameter or member) definition in the source code</p>
                """
    case TypeIdent             => """
                 <p>Type tree representing a reference to definition with a given name</p>
                """
    case TypeLambda            =>
      """
                 <p>Type of the definition of a type lambda taking a list of type parameters. It's return type may be a TypeLambda.</p>
                """
    case TypeParamClause       => """
                 <p>A type parameter clause <code>[X1, ..., Xn]</code></p>
                """
    case TypeProjection        => """
                 <p>Type tree representing a selection of definition with a given name on a given type prefix</p>
                """
    case TypeRef               => """
                 <p>Type of a reference to a type symbol</p>
                """
    case TypeRepr              => """
                 <p>A type, type constructors, type bounds or NoPrefix</p>
                """
    case TypeSelect            => """
                 <p>Type tree representing a selection of definition with a given name on a given term prefix</p>
                """
    case TypeTree              => """
                 <p>Type tree representing a type written in the source</p>
                """
    case Typed                 => """
                 <p>Tree representing a type ascription <code>x: T</code> in the source code.</p>
                """
    case TypedOrTest           => """
                 <p>Tree representing a type ascription or type test pattern <code>x: T</code> in the source code.</p>
                """
    case Unapply               => """
                 <p>Pattern representing a <code>Xyz(...)</code> unapply.</p>
                """
    case UnitConstant          => """
                 <p>Constant Unit value</p>
                """
    case ValDef                =>
      """
                 <p>Tree representing a value definition in the source code. This includes <code>val</code>, <code>lazy val</code>, <code>var</code>, <code>object</code> and parameter definitions.</p>
                """
    case ValOrDefDef           =>
      """
                 <p>Tree representing a value or method definition in the source code. This includes <code>def</code>, <code>val</code>, <code>lazy val</code>, <code>var</code>, <code>object</code> and parameter definitions.</p>
                """
    case While                 => """
                 <p>Tree representing a while loop</p>
                """
    case Wildcard              => """
                 <p>Pattern representing a <code>_</code> wildcard.</p>
                """
    case WildcardTypeTree      =>
      """
                 <p>Type tree representing wildcard type bounds written in the source. The wildcard type <code>_</code> (for example in in <code>List[_]</code>) will be a type tree that represents a type but has <code>TypeBounds</code> inside.</p>
                """

  def hasModuleDoc(tpe: ReflectionType): Boolean = tpe match
    case ReflectionType.OrType                => true
    case ReflectionType.TypeIdent             => true
    case ReflectionType.ShortConstant         => true
    case ReflectionType.AndType               => true
    case ReflectionType.FlexibleType          => true
    case ReflectionType.Flags                 => true
    case ReflectionType.Term                  => true
    case ReflectionType.FloatConstant         => true
    case ReflectionType.Return                => true
    case ReflectionType.LongConstant          => true
    case ReflectionType.NoPrefix              => true
    case ReflectionType.StringConstant        => true
    case ReflectionType.ByNameType            => true
    case ReflectionType.TypeBlock             => true
    case ReflectionType.TypeProjection        => true
    case ReflectionType.ParamRef              => true
    case ReflectionType.MethodOrPoly          => false
    case ReflectionType.ClassOfConstant       => true
    case ReflectionType.Constant              => true
    case ReflectionType.Repeated              => true
    case ReflectionType.ThisType              => true
    case ReflectionType.Typed                 => true
    case ReflectionType.Annotated             => true
    case ReflectionType.This                  => true
    case ReflectionType.MatchType             => true
    case ReflectionType.TypeLambda            => true
    case ReflectionType.Singleton             => true
    case ReflectionType.SourceFile            => true
    case ReflectionType.PackageClause         => true
    case ReflectionType.SimpleSelector        => true
    case ReflectionType.LambdaType            => false
    case ReflectionType.NullConstant          => true
    case ReflectionType.Inlined               => true
    case ReflectionType.CaseDef               => true
    case ReflectionType.RecursiveType         => true
    case ReflectionType.DefDef                => true
    case ReflectionType.ImplicitSearchFailure => false
    case ReflectionType.PolyType              => true
    case ReflectionType.ClassDef              => true
    case ReflectionType.MethodType            => true
    case ReflectionType.Position              => true
    case ReflectionType.Export                => true
    case ReflectionType.WildcardTypeTree      => true
    case ReflectionType.TypedOrTest           => true
    case ReflectionType.TypeApply             => true
    case ReflectionType.Tree                  => true
    case ReflectionType.ByName                => true
    case ReflectionType.Literal               => true
    case ReflectionType.CharConstant          => true
    case ReflectionType.OmitSelector          => true
    case ReflectionType.Inferred              => true
    case ReflectionType.LambdaTypeTree        => true
    case ReflectionType.Alternatives          => true
    case ReflectionType.NamedType             => false
    case ReflectionType.TypeBind              => true
    case ReflectionType.SelectOuter           => true
    case ReflectionType.TypeDef               => true
    case ReflectionType.Apply                 => true
    case ReflectionType.Applied               => true
    case ReflectionType.TypeBoundsTree        => true
    case ReflectionType.Signature             => true
    case ReflectionType.Selector              => true
    case ReflectionType.TypeParamClause       => true
    case ReflectionType.AndOrType             => false
    case ReflectionType.Definition            => true
    case ReflectionType.ImplicitSearchSuccess => false
    case ReflectionType.RecursiveThis         => true
    case ReflectionType.Try                   => true
    case ReflectionType.ValOrDefDef           => false
    case ReflectionType.IntConstant           => true
    case ReflectionType.AppliedType           => true
    case ReflectionType.MatchTypeTree         => true
    case ReflectionType.BooleanConstant       => true
    case ReflectionType.Statement             => false
    case ReflectionType.Wildcard              => true
    case ReflectionType.While                 => true
    case ReflectionType.TypeBounds            => true
    case ReflectionType.If                    => true
    case ReflectionType.SuperType             => true
    case ReflectionType.Refinement            => true
    case ReflectionType.Symbol                => true
    case ReflectionType.Import                => true
    case ReflectionType.Refined               => true
    case ReflectionType.RenameSelector        => true
    case ReflectionType.Unapply               => true
    case ReflectionType.AmbiguousImplicits    => false
    case ReflectionType.Super                 => true
    case ReflectionType.DivergingImplicit     => false
    case ReflectionType.ImplicitSearchResult  => false
    case ReflectionType.ByteConstant          => true
    case ReflectionType.NoMatchingImplicits   => false
    case ReflectionType.DoubleConstant        => true
    case ReflectionType.TypeRepr              => true
    case ReflectionType.SummonFrom            => true
    case ReflectionType.Bind                  => true
    case ReflectionType.ValDef                => true
    case ReflectionType.AnnotatedType         => true
    case ReflectionType.TypeTree              => true
    case ReflectionType.New                   => true
    case ReflectionType.NamedArg              => true
    case ReflectionType.Select                => true
    case ReflectionType.TermParamClause       => true
    case ReflectionType.ParamClause           => true
    case ReflectionType.Match                 => true
    case ReflectionType.TermRef               => true
    case ReflectionType.Closure               => true
    case ReflectionType.Block                 => true
    case ReflectionType.ConstantType          => true
    case ReflectionType.TypeRef               => true
    case ReflectionType.Assign                => true
    case ReflectionType.Ident                 => true
    case ReflectionType.TypeSelect            => true
    case ReflectionType.GivenSelector         => true
    case ReflectionType.Ref                   => true
    case ReflectionType.TypeCaseDef           => true
    case ReflectionType.MatchCase             => true
    case ReflectionType.UnitConstant          => true

  def hasMethodsDocs(tpe: ReflectionType): Boolean = tpe match
    case ReflectionType.OrType => true
    case ReflectionType.TypeIdent => true
    case ReflectionType.ShortConstant => false
    case ReflectionType.AndType => false
    case ReflectionType.FlexibleType => true
    case ReflectionType.Flags => true
    case ReflectionType.Term => true
    case ReflectionType.FloatConstant => false
    case ReflectionType.Return => true
    case ReflectionType.LongConstant => false
    case ReflectionType.NoPrefix => false
    case ReflectionType.StringConstant => false
    case ReflectionType.ByNameType => true
    case ReflectionType.TypeBlock => true
    case ReflectionType.TypeProjection => true
    case ReflectionType.ParamRef => true
    case ReflectionType.MethodOrPoly => false
    case ReflectionType.ClassOfConstant => false
    case ReflectionType.Constant => true
    case ReflectionType.Repeated => true
    case ReflectionType.ThisType => true
    case ReflectionType.Typed => true
    case ReflectionType.Annotated => true
    case ReflectionType.This => true
    case ReflectionType.MatchType => true
    case ReflectionType.TypeLambda => true
    case ReflectionType.Singleton => true
    case ReflectionType.SourceFile => true
    case ReflectionType.PackageClause => true
    case ReflectionType.SimpleSelector => true
    case ReflectionType.LambdaType => true
    case ReflectionType.NullConstant => false
    case ReflectionType.Inlined => true
    case ReflectionType.CaseDef => true
    case ReflectionType.RecursiveType => true
    case ReflectionType.DefDef => true
    case ReflectionType.ImplicitSearchFailure => true
    case ReflectionType.PolyType => true
    case ReflectionType.ClassDef => true
    case ReflectionType.MethodType => true
    case ReflectionType.Position => true
    case ReflectionType.Export => true
    case ReflectionType.WildcardTypeTree => true
    case ReflectionType.TypedOrTest => true
    case ReflectionType.TypeApply => true
    case ReflectionType.Tree => true
    case ReflectionType.ByName => true
    case ReflectionType.Literal => true
    case ReflectionType.CharConstant => false
    case ReflectionType.OmitSelector => true
    case ReflectionType.Inferred => false
    case ReflectionType.LambdaTypeTree => true
    case ReflectionType.Alternatives => true
    case ReflectionType.NamedType => true
    case ReflectionType.TypeBind => true
    case ReflectionType.SelectOuter => true
    case ReflectionType.TypeDef => true
    case ReflectionType.Apply => true
    case ReflectionType.Applied => true
    case ReflectionType.TypeBoundsTree => true
    case ReflectionType.Signature => true
    case ReflectionType.Selector => true
    case ReflectionType.TypeParamClause => true
    case ReflectionType.AndOrType => true
    case ReflectionType.Definition => true
    case ReflectionType.ImplicitSearchSuccess => true
    case ReflectionType.RecursiveThis => true
    case ReflectionType.Try => true
    case ReflectionType.ValOrDefDef => true
    case ReflectionType.IntConstant => false
    case ReflectionType.AppliedType => true
    case ReflectionType.MatchTypeTree => true
    case ReflectionType.BooleanConstant => false
    case ReflectionType.Statement => false
    case ReflectionType.Wildcard => false
    case ReflectionType.While => true
    case ReflectionType.TypeBounds => true
    case ReflectionType.If => true
    case ReflectionType.SuperType => true
    case ReflectionType.Refinement => true
    case ReflectionType.Symbol => true
    case ReflectionType.Import => true
    case ReflectionType.Refined => true
    case ReflectionType.RenameSelector => true
    case ReflectionType.Unapply => true
    case ReflectionType.AmbiguousImplicits => false
    case ReflectionType.Super => true
    case ReflectionType.DivergingImplicit => false
    case ReflectionType.ImplicitSearchResult => false
    case ReflectionType.ByteConstant => false
    case ReflectionType.NoMatchingImplicits => false
    case ReflectionType.DoubleConstant => false
    case ReflectionType.TypeRepr => true
    case ReflectionType.SummonFrom => true
    case ReflectionType.Bind => true
    case ReflectionType.ValDef => true
    case ReflectionType.AnnotatedType => true
    case ReflectionType.TypeTree => true
    case ReflectionType.New => true
    case ReflectionType.NamedArg => true
    case ReflectionType.Select => true
    case ReflectionType.TermParamClause => true
    case ReflectionType.ParamClause => true
    case ReflectionType.Match => true
    case ReflectionType.TermRef => false
    case ReflectionType.Closure => true
    case ReflectionType.Block => true
    case ReflectionType.ConstantType => true
    case ReflectionType.TypeRef => true
    case ReflectionType.Assign => true
    case ReflectionType.Ident => true
    case ReflectionType.TypeSelect => true
    case ReflectionType.GivenSelector => true
    case ReflectionType.Ref => true
    case ReflectionType.TypeCaseDef => true
    case ReflectionType.MatchCase => true
    case ReflectionType.UnitConstant => false
