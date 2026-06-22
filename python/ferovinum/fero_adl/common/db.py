# @generated from ADL module common.db

import enum
import pydantic
import typing


T = typing.TypeVar("T")

class DbTable(pydantic.BaseModel):
  tableName: str = pydantic.Field(default="")
  withIdPrimaryKey: bool = pydantic.Field(default=False)
  withPrimaryKey: list[str] = pydantic.Field(default=[])
  indexes: list[list[str]] = pydantic.Field(default=[])
  uniquenessConstraints: list[list[str]] = pydantic.Field(default=[])
  extraSql: list[str] = pydantic.Field(default=[])
  label: list[str] = pydantic.Field(default=[])

# Field level annotation to override the name of the
# database column.
type DbColumnName = str

# Field or type level annotation to override the type of the
# database column.
type DbColumnType = str

# Field level annotation to indicate that a column value
# is generated, and hence need not be requested from or edited
# by a user.
type DbColumnGenerated = None

# A reference for a database stored value, referenced by a
# string primary key.
class DbKey(pydantic.RootModel[str], typing.Generic[T]):
  pass

# A value of type T along with a unique db identifier
class WithDbId(pydantic.BaseModel, typing.Generic[T]):
  id: "DbKey[T]"
  value: T
