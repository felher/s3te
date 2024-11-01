package org.felher.s3te

final case class TreeData(
    code: Option[String],
    ast: Ast
) derives JsonEncoder
