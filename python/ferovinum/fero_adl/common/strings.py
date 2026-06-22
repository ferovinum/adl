# @generated from ADL module common.strings

import enum
import pydantic
import typing



# A string that isn't empty, and isn't only whitespace.
type StringNE = str

# An alphanumeric string, with hyphens for separation.
type StringANH = str

# A multi line, free-form text string
type StringML = str

# An email address
type EmailAddress = str

# A markdown text string
type StringMD = str
