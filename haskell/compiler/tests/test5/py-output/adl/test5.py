# @generated from ADL module test5

import enum
import pydantic
import typing



class U1(pydantic.BaseModel):
  class Type(enum.Enum):
    v = "v"

  type: Type
  value: typing.Optional[None] = pydantic.Field(default=None)


class U2(pydantic.BaseModel):
  class Type(enum.Enum):
    v = "v"

  type: Type
  value: typing.Optional[int] = pydantic.Field(default=None)


class U3(pydantic.BaseModel):
  class Type(enum.Enum):
    v = "v"

  type: Type
  value: typing.Optional[int] = pydantic.Field(default=None)


class S1(pydantic.BaseModel):
  f: int = pydantic.Field(default=100)


class U4(pydantic.BaseModel):
  class Type(enum.Enum):
    v = "v"

  type: Type
  value: typing.Optional[S1] = pydantic.Field(default=None)


class U5(pydantic.BaseModel):
  class Type(enum.Enum):
    v = "v"

  type: Type
  value: typing.Optional[S1] = pydantic.Field(default=None)


class U6(pydantic.BaseModel):
  class Type(enum.Enum):
    v = "v"

  type: Type
  value: typing.Optional[U3] = pydantic.Field(default=None)


class U7(pydantic.BaseModel):
  class Type(enum.Enum):
    v = "v"

  type: Type
  value: typing.Optional[U3] = pydantic.Field(default=None)


class U8(pydantic.BaseModel):
  class Type(enum.Enum):
    v1 = "v1"
    v2 = "v2"

  type: Type
  value: typing.Union[S1, int] = pydantic.Field(default=None)

T = typing.TypeVar('T')

class U9(pydantic.BaseModel, typing.Generic[T]):
  class Type(enum.Enum):
    v1 = "v1"
    v2 = "v2"
    v3 = "v3"

  type: Type
  value: typing.Union[T, int, None] = pydantic.Field(default=None)


class S(pydantic.BaseModel):
  f1: U9[str] = pydantic.Field(default=None)
  f2: U9[str] = pydantic.Field(default=None)
  f3: U9[str] = pydantic.Field(default=None)


class Cell(pydantic.BaseModel, typing.Generic[T]):
  head: T
  tail: "List[T]"


class List(pydantic.BaseModel, typing.Generic[T]):
  class Type(enum.Enum):
    null = "null"
    cell = "cell"

  type: Type
  value: typing.Union[None, Cell[T]] = pydantic.Field(default=None)


class U10(pydantic.BaseModel):
  class Type(enum.Enum):
    v1 = "v1"
    v2 = "v2"

  type: Type
  value: typing.Union[int, None] = pydantic.Field(default=None)


class S10(pydantic.BaseModel):
  f1: U10 = pydantic.Field(default=None)
  f2: typing.Optional[U10] = pydantic.Field(default=None)
  f3: U10 = pydantic.Field(default=None)
  f4: typing.Optional[U10] = pydantic.Field(default=None)


class U11(pydantic.BaseModel):
  class Type(enum.Enum):
    v1 = "VALUE1"
    v2 = "VALUE2"

  type: Type
  value: typing.Union[int, None] = pydantic.Field(default=None)


class S11(pydantic.BaseModel):
  f1: U11 = pydantic.Field(default=None)
  f2: typing.Optional[U11] = pydantic.Field(default=None)
  f3: U11 = pydantic.Field(default=None)
  f4: typing.Optional[U11] = pydantic.Field(default=None) 