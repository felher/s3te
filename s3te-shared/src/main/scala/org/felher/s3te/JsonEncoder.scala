package org.felher.s3te

import scala.deriving.*
import scala.compiletime.*

trait JsonEncoder[A]:
  def encode(a: A): String

  def contramap[B](f: B => A): JsonEncoder[B] =
    a => encode(f(a))

object JsonEncoder:
  def encode[A](a: A)(using enc: JsonEncoder[A]): String =
    enc.encode(a)

  given intEncoder: JsonEncoder[Int] with
    def encode(a: Int): String = a.toString

  given stringEncoder: JsonEncoder[String] with
    def encode(a: String): String = Util.enquote(a)

  given booleanEncoder: JsonEncoder[Boolean] with
    def encode(a: Boolean): String = if a then "true" else "false"

  given mapEncoder[V](using vEnc: JsonEncoder[V]): JsonEncoder[Map[String, V]] =
    new JsonEncoder[Map[String, V]]:
      override def encode(a: Map[String, V]): String =
        obj(a.toList.map((k, v) => (k, vEnc.encode(v)))*)

  given optionEncoder[V](using vEnc: JsonEncoder[V]): JsonEncoder[Option[V]] =
    new JsonEncoder[Option[V]]:
      override def encode(a: Option[V]): String =
        a.fold("null")(v => vEnc.encode(v))

  given listEncoder[V](using vEnc: JsonEncoder[V]): JsonEncoder[List[V]] =
    new JsonEncoder[List[V]]:
      override def encode(a: List[V]): String =
        array(a.map(vEnc.encode)*)

  def array(values: String*): String =
    values.mkString("[", ", ", "]")

  def obj(fields: (String, String)*): String =
    fields.map((k, v) => s"${Util.enquote(k)}: $v").mkString("{", ", ", "}")

  private def product[T](fields: => List[(String, JsonEncoder[Any])]): JsonEncoder[T] =
    new JsonEncoder[T]:
      override def encode(a: T): String =
        val values        = a.asInstanceOf[Product].productIterator.toList
        val encoders      = fields.map(_._2)
        val encodedValues = encoders.zip(values).map((e, v) => e.encode(v))
        val names         = fields.map(_._1)
        obj(names.zip(encodedValues)*)

  private def sum[T](ordinal: T => Int, encoders: => List[JsonEncoder[Any]]): JsonEncoder[T] =
    new JsonEncoder[T]:
      override def encode(t: T): String =
        encoders(ordinal(t)).encode(t)

  inline def derived[T](using m: Mirror.Of[T]): JsonEncoder[T] =
    inline m match
      case p: Mirror.ProductOf[T] =>
        val name         = constValue[p.MirroredLabel]
        val elementNames = constValueTuple[p.MirroredElemLabels]
        inline if elementNames.size == 0 then stringEncoder.contramap[T](_ => name)
        else
          lazy val subEncoders = summonInlineAll[p.MirroredElemTypes]
          val namedEncs        = elementNames.toList.zip(subEncoders).asInstanceOf[List[(String, JsonEncoder[Any])]]
          product[T](namedEncs)

      case s: Mirror.SumOf[T] =>
        lazy val childEncoders = deriveAll[s.MirroredElemTypes]
        sum[T](s.ordinal, childEncoders.asInstanceOf[List[JsonEncoder[Any]]])

  private inline def deriveAll[T <: Tuple]: List[JsonEncoder[Any]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   =>
        derived[h](using summonInline).asInstanceOf[JsonEncoder[Any]] :: deriveAll[t]

  private inline def summonInlineAll[T <: Tuple]: List[JsonEncoder[Any]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   =>
        summonInline[JsonEncoder[h]].asInstanceOf[JsonEncoder[Any]] :: summonInlineAll[t]
