# @generated from ADL module sys.annotations

import enum
import pydantic
import typing



type Doc = str

type SerializedName = str

class SerializedWithInternalTag(pydantic.BaseModel):
  tag: str

type CustomSerialization = bool
