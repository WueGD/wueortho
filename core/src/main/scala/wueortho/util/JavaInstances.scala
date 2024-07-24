package wueortho.util

import java.util.UUID

object JavaInstances:
  given CanEqual[UUID, UUID] = CanEqual.derived
