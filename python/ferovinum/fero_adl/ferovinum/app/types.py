# @generated from ADL module ferovinum.app.types

import enum
import pydantic
import typing

import fero_adl.common.strings as common_strings


# A value that represents money, i.e. has two decimal places.
class MonetaryValue(pydantic.RootModel[common_strings.StringNE]):
  pass

class Decimal(pydantic.RootModel[common_strings.StringNE]):
  pass
