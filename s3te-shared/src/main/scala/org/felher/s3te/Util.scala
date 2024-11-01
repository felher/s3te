package org.felher.s3te

object Util:
  def escapeString(s: String): String =
    s.flatMap:
      case '"'          => "\\\""
      case '\\'         => "\\\\"
      case c if c < ' ' => "\\u%04x".format(c.toInt)
      case c            => c.toString

  def enquote(s: String): String =
    "\"" + escapeString(s) + "\""

  def decapitalize(s: String): String =
    s"${s.head.toLower}${s.tail}"
