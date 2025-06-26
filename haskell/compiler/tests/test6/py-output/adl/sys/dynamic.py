# @generated from ADL module sys.dynamic

import enum
import pydantic
import typing

import adl.sys.adlast as sys_adlast


# A serialised value along with  its type
class Dynamic(pydantic.BaseModel):
  typeExpr: sys_adlast.TypeExpr
  value: dict | None
