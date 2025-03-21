# @generated from ADL module test6

import enum
import pydantic
import typing

import adl.sys.types as sys_types


class S(pydantic.BaseModel):
  f_pair: sys_types.Pair[int, float]
  f_either: sys_types.Either[str, int]
  f_map: sys_types.Map[str, float]
  f_set: sys_types.Set[str]
  f_mstring: sys_types.Maybe[str]
  f_mstring2: sys_types.Maybe[str] = pydantic.Field(default={"just": "sukpeepolup"})
  f_nstring: typing.Union[str, None]
  f_nstring2: typing.Union[str, None] = pydantic.Field(default="abcde")
  f_int: typing.Union[int, None]
  f_int2: typing.Union[int, None] = pydantic.Field(default=100)
  f_int3: typing.Union[int, None] = pydantic.Field(default=None)
