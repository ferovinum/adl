# @generated from ADL module common.ui

import enum
import pydantic
import typing

import fero_adl.sys.types as sys_types


type FormLabel = str

type FormDescription = str

type FormGroupKey = str

class FormGroups(pydantic.BaseModel):
  defaultKey: "FormGroupKey"
  labels: list[sys_types.Pair["FormGroupKey", str]]

# An field/type alias annotation to constrain the
# values allowed by a string to the enumerated values
class ValidValues(pydantic.BaseModel):
  # The allowed values
  values: list[str]
  # A (short) user readable string describing the
  # expected text.
  description: str

# An field/type alias annotation to constrain the
# values allowed by a string to a regular expression
class ValidRegex(pydantic.BaseModel):
  # The regexp that must be matched
  regex: str
  # A (short) user readable string describing the
  # expected text.
  description: str
  # The regex group index to return if matches
  # 0 is the entire string
  returnGroup: int = pydantic.Field(default=0)
