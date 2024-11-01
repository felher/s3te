package org.felher.s3te

enum PathEntry derives JsonEncoder:
  case ExtractorChild(index: Int)
  case MethodCall(key: MethodKey[?])

object PathEntry:
  given Ordering[PathEntry] = Ordering[(Int, Int)].on[PathEntry]:
    case ExtractorChild(i) => (0, i)
    case MethodCall(key)   => (1, key.ordinal)
