# @generated from ADL module test29

import enum
import pydantic
import typing



# An example with weird "quoting" conventions, designed to break things
class Test(pydantic.BaseModel):
  # "foo" as a field
  foo: dict[str, str] = pydantic.Field(default={" ": "baz", "\"": "baz", "$": "bar", "'": "baz", "degrees": "°"})
