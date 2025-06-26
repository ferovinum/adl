# @generated from ADL module test31

import enum
import pydantic
import typing



class ValidRegex(pydantic.BaseModel):
  # The regular expression pattern that validates the string
  regex: str
  # A short description of what the pattern validates
  description: str

# A URL string that must be valid
Url: typing.TypeAlias = typing.Annotated[str, pydantic.StringConstraints(pattern=r"^(https?|ftp)://[^\s/$.?#].[^\s]*$"), "a valid URL"]

# A date in YYYY-MM-DD format
IsoDate: typing.TypeAlias = typing.Annotated[str, pydantic.StringConstraints(pattern=r"^\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12]\d|3[01])$"), "date in YYYY-MM-DD format"]

# A phone number in international format
PhoneNumber: typing.TypeAlias = typing.Annotated[str, pydantic.StringConstraints(pattern=r"^\+[1-9]\d{1,14}$"), "international phone number"]

# A simple IP address (v4) validator
IPv4Address: typing.TypeAlias = typing.Annotated[str, pydantic.StringConstraints(pattern=r"^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"), "IPv4 address"]

# A username (alphanumeric with underscore, 3-16 chars)
Username: typing.TypeAlias = typing.Annotated[str, pydantic.StringConstraints(pattern=r"^[a-zA-Z0-9_]{3,16}$"), "alphanumeric username with underscore, 3-16 chars"]

# A string with complex regex with escaping and quotes
ComplexPattern: typing.TypeAlias = typing.Annotated[str, pydantic.StringConstraints(pattern=r"""^[a-z0-9]([a-z0-9\-\."']*[a-z0-9])?$"""), "string with special characters including quotes"]
