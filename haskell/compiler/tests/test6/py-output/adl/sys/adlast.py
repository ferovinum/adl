# @generated from ADL module sys.adlast

import enum
import pydantic
import typing

import adl.sys.types as sys_types


ModuleName: typing.TypeAlias = str

Ident: typing.TypeAlias = str

Annotations: typing.TypeAlias = sys_types.Map["ScopedName", dict | None]

class ScopedName(pydantic.BaseModel):
  moduleName: "ModuleName"
  name: "Ident"

class TypeRef_Primitive(pydantic.BaseModel):
  primitive: "Ident"

class TypeRef_TypeParam(pydantic.BaseModel):
  typeParam: "Ident"

class TypeRef_Reference(pydantic.BaseModel):
  reference: "ScopedName"

class TypeRef(pydantic.RootModel[typing.Union[TypeRef_Primitive | TypeRef_TypeParam | TypeRef_Reference]]):
  pass

class TypeExpr(pydantic.BaseModel):
  typeRef: "TypeRef"
  parameters: list["TypeExpr"]

class Field(pydantic.BaseModel):
  name: "Ident"
  serializedName: "Ident"
  typeExpr: "TypeExpr"
  default: sys_types.Maybe[dict | None]
  annotations: "Annotations"

class Struct(pydantic.BaseModel):
  typeParams: list["Ident"]
  fields: list["Field"]

class Union(pydantic.BaseModel):
  typeParams: list["Ident"]
  fields: list["Field"]

class TypeDef(pydantic.BaseModel):
  typeParams: list["Ident"]
  typeExpr: "TypeExpr"

class NewType(pydantic.BaseModel):
  typeParams: list["Ident"]
  typeExpr: "TypeExpr"
  default: sys_types.Maybe[dict | None]

class DeclType_Struct_(pydantic.BaseModel):
  struct_: "Struct"

class DeclType_Union_(pydantic.BaseModel):
  union_: "Union"

class DeclType_Type_(pydantic.BaseModel):
  type_: "TypeDef"

class DeclType_Newtype_(pydantic.BaseModel):
  newtype_: "NewType"

class DeclType(pydantic.RootModel[typing.Union[DeclType_Struct_ | DeclType_Union_ | DeclType_Type_ | DeclType_Newtype_]]):
  pass

class Decl(pydantic.BaseModel):
  name: "Ident"
  version: sys_types.Maybe[int]
  type_: "DeclType"
  annotations: "Annotations"

class ScopedDecl(pydantic.BaseModel):
  moduleName: "ModuleName"
  decl: "Decl"

DeclVersions: typing.TypeAlias = list["Decl"]

class Import_ModuleName(pydantic.BaseModel):
  moduleName: "ModuleName"

class Import_ScopedName(pydantic.BaseModel):
  scopedName: "ScopedName"

class Import(pydantic.RootModel[typing.Union[Import_ModuleName | Import_ScopedName]]):
  pass

class Module(pydantic.BaseModel):
  name: "ModuleName"
  imports: list["Import"]
  decls: dict[str, "Decl"]
  annotations: "Annotations"
