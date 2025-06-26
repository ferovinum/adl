# @generated from ADL module test30

import enum
import pydantic
import typing



class ValidRegex(pydantic.BaseModel):
  # The regexp that must be matched
  regex: str
  # A (short) user readable string describing the
  # expected text.
  description: str
  # The regex group index to return if matches
  # 0 is the entire string
  returnGroup: int = pydantic.Field(default=0)

# A string that isn't empty, and isn't only whitespace.
StringNE: typing.TypeAlias = typing.Annotated[str, pydantic.StringConstraints(pattern=r"^.*\S+.*$"), "non empty"]

# An alphanumeric string, with hyphens for separation.
StringANH: typing.TypeAlias = typing.Annotated[str, pydantic.StringConstraints(pattern=r"^[A-Za-z][A-Za-z0-9-]*$"), "alphanumeric"]

# An email address
EmailAddress: typing.TypeAlias = typing.Annotated[str, pydantic.StringConstraints(pattern=r"""^\s*((?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\]))\s*$"""), "an email address"]

# A string type with no annotation (for comparison)
StringRegular: typing.TypeAlias = str
