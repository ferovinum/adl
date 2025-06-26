# @generated from ADL module test20

import enum
import pydantic
import typing



class Role(pydantic.BaseModel):
  class Type(enum.Enum):
    underling = "u"
    boss = "b"
    superBoss = "sb"

  type: Type
  value: typing.Optional[None] = pydantic.Field(default=None)


class Person(pydantic.BaseModel):
  firstName: str = pydantic.Field(serialization_alias="fn")
  lastName: str = pydantic.Field(serialization_alias="ln")
  age: int
  role: Role 