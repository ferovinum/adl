# @generated from ADL module ferovinum.app.annotations

import enum
import pydantic
import typing



class DbAudit(pydantic.BaseModel):
  auditRows: bool
  auditQueryText: bool
