# @generated from ADL module sys.types

import enum
import pydantic
import typing


E = typing.TypeVar("E")
K = typing.TypeVar("K")
T = typing.TypeVar("T")
T1 = typing.TypeVar("T1")
T2 = typing.TypeVar("T2")
V = typing.TypeVar("V")

class Pair(pydantic.BaseModel, typing.Generic[T1, T2]):
  v1: T1
  v2: T2

class Either_Left(pydantic.BaseModel, typing.Generic[T1]):
  left: T1

class Either_Right(pydantic.BaseModel, typing.Generic[T2]):
  right: T2

class Either(pydantic.RootModel[typing.Union[Either_Left[T1] | Either_Right[T2]]], typing.Generic[T1, T2]):
  pass

class Maybe_Nothing(pydantic.BaseModel):
  nothing: None

class Maybe_Just(pydantic.BaseModel, typing.Generic[T]):
  just: T

class Maybe(pydantic.RootModel[typing.Union[Maybe_Nothing | Maybe_Just[T]]], typing.Generic[T]):
  pass

class Result_Ok(pydantic.BaseModel, typing.Generic[T]):
  ok: T

class Result_Error(pydantic.BaseModel, typing.Generic[E]):
  error: E

class Result(pydantic.RootModel[typing.Union[Result_Ok[T] | Result_Error[E]]], typing.Generic[T, E]):
  pass

class MapEntry(pydantic.BaseModel, typing.Generic[K, V]):
  key: K
  value: V

class Map(pydantic.RootModel[list["MapEntry[K, V]"]], typing.Generic[K, V]):
  pass

class Set(pydantic.RootModel[list[T]], typing.Generic[T]):
  pass
