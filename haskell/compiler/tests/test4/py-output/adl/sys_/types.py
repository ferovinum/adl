# @generated from ADL module sys.types

import enum
import pydantic
import typing

T1 = typing.TypeVar('T1')
T2 = typing.TypeVar('T2')

class Pair(pydantic.BaseModel, typing.Generic[T1, T2]):
  v1: T1
  v2: T2


Maybe = typing.Optional

Set = typing.Set

Map = dict 