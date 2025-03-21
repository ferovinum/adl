# @generated from ADL module test4

import enum
import pydantic
import typing

from . import sys_
from .sys_ import types


Date: typing.TypeAlias = str


class CDate(pydantic.BaseModel):
  year: int
  month: int
  day: int


class S(pydantic.BaseModel):
  v1: Date
  v2: Date = pydantic.Field(default="2000-01-01")
  v3: CDate
  v4: CDate = pydantic.Field(default=CDate(year=2000, month=1, day=1))
  v5: typing.Optional[str]
  v5a: typing.Optional[str] = pydantic.Field(default=None)
  v5b: typing.Optional[str] = pydantic.Field(default="hello")
  v6: types.Pair[str, int]
  v7: typing.Set[int] = pydantic.Field(default={1, 2, 3})
  v7a: typing.Set[int]
  v8: dict[str, int]
  v8a: dict[str, int] = pydantic.Field(default={"X": 1, "Y": 2})


class S2(pydantic.BaseModel):
  intv: int 