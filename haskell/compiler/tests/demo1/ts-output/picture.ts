/* Automatically generated by adlc */

import * as ADL from './runtime/adl';

export interface Circle {
  radius: number;
}

export function makeCircle(
  input: {
    radius: number,
  }
): Circle {
  return {
    radius: input.radius,
  };
}

const Circle_AST : ADL.ScopedDecl =
  {"moduleName":"picture","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"radius","default":{"kind":"nothing"},"name":"radius","typeExpr":{"typeRef":{"kind":"primitive","value":"Double"},"parameters":[]}}]}},"name":"Circle","version":{"kind":"nothing"}}};

export function texprCircle(): ADL.ATypeExpr<Circle> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "picture",name : "Circle"}}, parameters : []}};
}

interface Picture_Circle {
  kind: 'circle';
  value: Circle;
}
interface Picture_Composed {
  kind: 'composed';
  value: Picture[];
}
interface Picture_Rectangle {
  kind: 'rectangle';
  value: Rectangle;
}
interface Picture_Translated {
  kind: 'translated';
  value: Translated<Picture>;
}

export type Picture = Picture_Circle | Picture_Composed | Picture_Rectangle | Picture_Translated;

const Picture_AST : ADL.ScopedDecl =
  {"moduleName":"picture","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"circle","default":{"kind":"nothing"},"name":"circle","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"picture","name":"Circle"}},"parameters":[]}},{"annotations":[],"serializedName":"rectangle","default":{"kind":"nothing"},"name":"rectangle","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"picture","name":"Rectangle"}},"parameters":[]}},{"annotations":[],"serializedName":"composed","default":{"kind":"nothing"},"name":"composed","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"picture","name":"Picture"}},"parameters":[]}]}},{"annotations":[],"serializedName":"translated","default":{"kind":"nothing"},"name":"translated","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"picture","name":"Translated"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"picture","name":"Picture"}},"parameters":[]}]}}]}},"name":"Picture","version":{"kind":"nothing"}}};

export function texprPicture(): ADL.ATypeExpr<Picture> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "picture",name : "Picture"}}, parameters : []}};
}

export interface Rectangle {
  height: number;
  width: number;
}

export function makeRectangle(
  input: {
    height: number,
    width: number,
  }
): Rectangle {
  return {
    height: input.height,
    width: input.width,
  };
}

const Rectangle_AST : ADL.ScopedDecl =
  {"moduleName":"picture","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"width","default":{"kind":"nothing"},"name":"width","typeExpr":{"typeRef":{"kind":"primitive","value":"Double"},"parameters":[]}},{"annotations":[],"serializedName":"height","default":{"kind":"nothing"},"name":"height","typeExpr":{"typeRef":{"kind":"primitive","value":"Double"},"parameters":[]}}]}},"name":"Rectangle","version":{"kind":"nothing"}}};

export function texprRectangle(): ADL.ATypeExpr<Rectangle> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "picture",name : "Rectangle"}}, parameters : []}};
}

export interface Translated<T> {
  object: T;
  xoffset: number;
  yoffset: number;
}

export function makeTranslated<T>(
  input: {
    object: T,
    xoffset?: number,
    yoffset?: number,
  }
): Translated<T> {
  return {
    object: input.object,
    xoffset: input.xoffset === undefined ? 0 : input.xoffset,
    yoffset: input.yoffset === undefined ? 0 : input.yoffset,
  };
}

const Translated_AST : ADL.ScopedDecl =
  {"moduleName":"picture","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"xoffset","default":{"kind":"just","value":{"kind":"integer","value":0}},"name":"xoffset","typeExpr":{"typeRef":{"kind":"primitive","value":"Double"},"parameters":[]}},{"annotations":[],"serializedName":"yoffset","default":{"kind":"just","value":{"kind":"integer","value":0}},"name":"yoffset","typeExpr":{"typeRef":{"kind":"primitive","value":"Double"},"parameters":[]}},{"annotations":[],"serializedName":"object","default":{"kind":"nothing"},"name":"object","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}}]}},"name":"Translated","version":{"kind":"nothing"}}};

export function texprTranslated<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Translated<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "picture",name : "Translated"}}, parameters : [texprT.value]}};
}

export const _AST_MAP = {
  "picture.Circle" : Circle_AST,
  "picture.Picture" : Picture_AST,
  "picture.Rectangle" : Rectangle_AST,
  "picture.Translated" : Translated_AST
};