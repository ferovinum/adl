# @generated from ADL module test14

import enum
import pydantic
import typing



class switch_(pydantic.BaseModel):
  double_: float
  int_: int
  string: str
  for_: bool
  Objects: str


class unsigned_(pydantic.BaseModel):
  class Type(enum.Enum):
    null = "null"

  type: Type
  value: typing.Optional[None] = pydantic.Field(default=None)


class Factory(pydantic.BaseModel):
  pass 