package org.felher.s3te.ui.float

import org.felher.s3te.ui.geo.Direction

final case class Positioning(
    allowedDirections: List[Direction],
    useOverlay: Boolean,
)
