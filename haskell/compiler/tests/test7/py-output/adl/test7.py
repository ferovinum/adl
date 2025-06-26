# @generated from ADL module test7

import enum
import pydantic
import typing


T = typing.TypeVar('T')

class Point(pydantic.BaseModel, typing.Generic[T]):
  x: T
  y: T


Int1: typing.TypeAlias = int

class Int2(pydantic.BaseModel):
  pass


class Int3(pydantic.BaseModel):
  pass


X = typing.TypeVar('X')

Int4: typing.TypeAlias = int

class Int5(pydantic.BaseModel, typing.Generic[X]):
  pass


class Int6(pydantic.BaseModel, typing.Generic[X]):
  pass


String1: typing.TypeAlias = str

class String2(pydantic.BaseModel):
  pass


class String3(pydantic.BaseModel):
  pass


String4: typing.TypeAlias = str

class String5(pydantic.BaseModel, typing.Generic[X]):
  pass


class String6(pydantic.BaseModel, typing.Generic[X]):
  pass


IntPoint1: typing.TypeAlias = Point[int]

class IntPoint2(pydantic.BaseModel):
  pass


class IntPoint3(pydantic.BaseModel):
  pass


Point1: typing.TypeAlias = Point[X]

class Point2(pydantic.BaseModel, typing.Generic[X]):
  pass


IntPoint1A: typing.TypeAlias = IntPoint1


class S(pydantic.BaseModel):
  f1: IntPoint1A 