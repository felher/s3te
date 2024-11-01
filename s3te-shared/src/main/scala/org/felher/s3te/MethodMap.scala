package org.felher.s3te

final case class MethodMap(map: Map[MethodKey[?], Any]):
  def add[V](key: MethodKey[V], v: V): MethodMap = MethodMap(map + (key -> v))
  def get[V](key: MethodKey[V]): Option[V]       = map.get(key).asInstanceOf[Option[V]]

object MethodMap:
  val empty = MethodMap(Map.empty)
  given JsonEncoder[MethodMap] with
    def encode(mm: MethodMap) =
      JsonEncoder.obj(
        mm.map.toList.map((k, v) => (k.toString, MethodKey.valueEncoder(k).asInstanceOf[JsonEncoder[Any]].encode(v)))*
      )
