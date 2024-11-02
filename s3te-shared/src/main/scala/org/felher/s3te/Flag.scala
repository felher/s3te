package org.felher.s3te

import scala.quoted.Quotes

enum Flag derives CanEqual, JsonEncoder:
  case Abstract
  case Artifact
  case Case
  case CaseAccessor
  case Contravariant
  case Covariant
  case Deferred
  case Enum
  case Erased
  case Exported
  case ExtensionMethod
  case FieldAccessor
  case Final
  case Given
  case HasDefault
  case Implicit
  case Infix
  case Inline
  case Invisible
  case JavaDefined
  case JavaStatic
  case Lazy
  case Local
  case Macro
  case Method
  case Module
  case Mutable
  case NoInits
  case Opaque
  case Open
  case Override
  case Package
  case Param
  case ParamAccessor
  case Private
  case PrivateLocal
  case Protected
  case Scala2x
  case Sealed
  case StableRealizable
  case Synthetic
  case Trait
  case Transparent

  def toCompilerFlags(using q: Quotes): q.reflect.Flags =
    this match
      case Abstract         => q.reflect.Flags.Abstract
      case Artifact         => q.reflect.Flags.Artifact
      case Case             => q.reflect.Flags.Case
      case CaseAccessor     => q.reflect.Flags.CaseAccessor
      case Contravariant    => q.reflect.Flags.Contravariant
      case Covariant        => q.reflect.Flags.Covariant
      case Deferred         => q.reflect.Flags.Deferred
      case Enum             => q.reflect.Flags.Enum
      case Erased           => q.reflect.Flags.Erased
      case Exported         => q.reflect.Flags.Exported
      case ExtensionMethod  => q.reflect.Flags.ExtensionMethod
      case FieldAccessor    => q.reflect.Flags.FieldAccessor
      case Final            => q.reflect.Flags.Final
      case Given            => q.reflect.Flags.Given
      case HasDefault       => q.reflect.Flags.HasDefault
      case Implicit         => q.reflect.Flags.Implicit
      case Infix            => q.reflect.Flags.Infix
      case Inline           => q.reflect.Flags.Inline
      case Invisible        => q.reflect.Flags.Invisible
      case JavaDefined      => q.reflect.Flags.JavaDefined
      case JavaStatic       => q.reflect.Flags.JavaStatic
      case Lazy             => q.reflect.Flags.Lazy
      case Local            => q.reflect.Flags.Local
      case Macro            => q.reflect.Flags.Macro
      case Method           => q.reflect.Flags.Method
      case Module           => q.reflect.Flags.Module
      case Mutable          => q.reflect.Flags.Mutable
      case NoInits          => q.reflect.Flags.NoInits
      case Opaque           => q.reflect.Flags.Opaque
      case Open             => q.reflect.Flags.Open
      case Override         => q.reflect.Flags.Override
      case Package          => q.reflect.Flags.Package
      case Param            => q.reflect.Flags.Param
      case ParamAccessor    => q.reflect.Flags.ParamAccessor
      case Private          => q.reflect.Flags.Private
      case PrivateLocal     => q.reflect.Flags.PrivateLocal
      case Protected        => q.reflect.Flags.Protected
      case Scala2x          => q.reflect.Flags.Scala2x
      case Sealed           => q.reflect.Flags.Sealed
      case StableRealizable => q.reflect.Flags.StableRealizable
      case Synthetic        => q.reflect.Flags.Synthetic
      case Trait            => q.reflect.Flags.Trait
      case Transparent      => q.reflect.Flags.Transparent

  def getScaladocs: String = this match
    case Abstract         => "Is this symbol `abstract`"
    case Artifact         => "Is this generated by Scala compiler. Corresponds to ACC_SYNTHETIC in the JVM."
    case Case             => "Is this symbol `case`"
    case CaseAccessor     => "Is this symbol a getter for case class parameter"
    case Contravariant    => "Is this symbol a type parameter marked as contravariant `-`"
    case Covariant        => "Is this symbol a type parameter marked as covariant `+`"
    case Deferred         => "Is a declared, but not defined member"
    case Enum             => "Is this symbol an enum"
    case Erased           => "Is this symbol `erased`"
    case Exported         => "Is this symbol exported from provided instance"
    case ExtensionMethod  => "Is this symbol a `def` defined in an `extension`"
    case FieldAccessor    => "Is this symbol a getter or a setter"
    case Final            => "Is this symbol `final`"
    case Given            => "Is this symbol an inferable ('given') parameter"
    case HasDefault       => "Is this symbol a parameter with a default value?"
    case Implicit         => "Is this symbol `implicit`"
    case Infix            => "Is an infix method or type"
    case Inline           => "Is this symbol `inline`"
    case Invisible        => "Is this symbol invisible when typechecking?"
    case JavaDefined      => "Is this symbol defined in a Java class"
    case JavaStatic       => "Is implemented as a Java static"
    case Lazy             => "Is this symbol `lazy`"
    case Local            =>
      "Is this symbol local? Used in conjunction with private/private[T] to mean private[this] extends Modifier protected[this]"
    case Macro            => "Is this symbol marked as a macro. An inline method containing top level splices"
    case Method           => "Is this symbol `def`"
    case Module           =>
      "Is this symbol an object or its class (used for a ValDef or a ClassDef extends Modifier respectively)"
    case Mutable          => "Is this symbol a `var` (when used on a ValDef)"
    case NoInits          => "Trait does not have fields or initialization code."
    case Opaque           => "Is this symbol `opaque`"
    case Open             => "Is this symbol `open`"
    case Override         => "Is this symbol `override`"
    case Package          => "Is this symbol a package"
    case Param            => "Is this symbol a parameter"
    case ParamAccessor    => "Is this symbol a parameter accessor"
    case Private          => "Is this symbol `private`"
    case PrivateLocal     => "Is this symbol labeled private[this]"
    case Protected        => "Is this symbol `protected`"
    case Scala2x          => "Was this symbol imported from Scala2.x"
    case Sealed           => "Is this symbol `sealed`"
    case StableRealizable => "Is this symbol member that is assumed to be stable and realizable"
    case Synthetic        => "Is this symbol to be tagged Java Synthetic"
    case Trait            => "Is this symbol a trait"
    case Transparent      => "Is a transparent inline method or trait"
