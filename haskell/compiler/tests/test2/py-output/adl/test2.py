# @generated from ADL module test2

import enum
import pydantic
import typing


T = typing.TypeVar("T")

# An empty structure.
class S0(pydantic.BaseModel):
  pass

# A structure containing primitives.
# It has two fields: an integer x and a String y.
class S1(pydantic.BaseModel):
  x: int
  y: str

# A structure containing a vector.
class S2(pydantic.BaseModel):
  f1: str
  f2: float
  f3: list[int]

# A generic structure.
class S3(pydantic.BaseModel, typing.Generic[T]):
  f1: str
  f2: float
  f3: T
  f4: list[T]

class S4(pydantic.BaseModel, typing.Generic[T]):
  f1: "S3[str]"
  f2: "S3[T]"

class Tree(pydantic.BaseModel, typing.Generic[T]):
  value: T
  children: list["Tree[T]"]

IntTree: typing.TypeAlias = "Tree[int]"
