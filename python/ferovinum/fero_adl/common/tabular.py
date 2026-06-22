# @generated from ADL module common.tabular

import enum
import pydantic
import typing

import fero_adl.common as common
import fero_adl.common.strings as common_strings

T = typing.TypeVar("T")

type FieldName = common_strings.StringNE

class ExprLike(pydantic.BaseModel):
  expr: "Expr"
  pattern: str
  caseSensitive: bool = pydantic.Field(default=True)

class ExprIn(pydantic.BaseModel):
  expr: "Expr"
  exprs: list["Expr"]

class ExprComparison(pydantic.BaseModel):
  expr1: "Expr"
  expr2: "Expr"

class FieldPredicate_EqualTo(pydantic.BaseModel):
  equalTo: "ExprComparison"

class FieldPredicate_In(pydantic.BaseModel):
  in_: "ExprIn"

class FieldPredicate_Like(pydantic.BaseModel):
  like: "ExprLike"

class FieldPredicate_Isnull(pydantic.BaseModel):
  isnull: "Expr"

class FieldPredicate_Not(pydantic.BaseModel):
  not_: "FieldPredicate"

class FieldPredicate_GreaterThan(pydantic.BaseModel):
  greaterThan: "ExprComparison"

class FieldPredicate_LessThan(pydantic.BaseModel):
  lessThan: "ExprComparison"

class FieldPredicate_And(pydantic.BaseModel):
  and_: list["FieldPredicate"]

class FieldPredicate_Or(pydantic.BaseModel):
  or_: list["FieldPredicate"]

class FieldPredicate_Literal(pydantic.BaseModel):
  literal: bool

class FieldPredicate(pydantic.RootModel[typing.Union[FieldPredicate_EqualTo | FieldPredicate_In | FieldPredicate_Like | FieldPredicate_Isnull | FieldPredicate_Not | FieldPredicate_GreaterThan | FieldPredicate_LessThan | FieldPredicate_And | FieldPredicate_Or | FieldPredicate_Literal]]):
  pass

class Expr_String(pydantic.BaseModel):
  string: str

class Expr_Int(pydantic.BaseModel):
  int: int

class Expr_Bool(pydantic.BaseModel):
  bool: bool

class Expr_Date(pydantic.BaseModel):
  date: common.LocalDate

class Expr_Instant(pydantic.BaseModel):
  instant: common.Instant

class Expr_Field(pydantic.BaseModel):
  field: "FieldName"

class Expr_CurrentDate(pydantic.BaseModel):
  currentDate: None

class Expr_Concat(pydantic.BaseModel):
  concat: list["Expr"]

class Expr(pydantic.RootModel[typing.Union[Expr_String | Expr_Int | Expr_Bool | Expr_Date | Expr_Instant | Expr_Field | Expr_CurrentDate | Expr_Concat]]):
  pass

class SortDirection(str, enum.Enum):
  ascending: str = "ascending"
  descending: str = "descending"

class SortField(pydantic.BaseModel):
  field: "FieldName"
  direction: "SortDirection"

class TableView(pydantic.BaseModel):
  columns: list["FieldName"]
  filter: "FieldPredicate" = pydantic.Field(default={"literal": True})
  sorting: list["SortField"] = pydantic.Field(default=[])

class TableQuery(pydantic.BaseModel):
  filter: "FieldPredicate" = pydantic.Field(default={"literal": True})
  sorting: list["SortField"] = pydantic.Field(default=[])
  offset: int = pydantic.Field(default=0)
  count: int = pydantic.Field(default=-1)

class SingleField(pydantic.BaseModel, typing.Generic[T]):
  value: T
