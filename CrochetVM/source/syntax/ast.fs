module Crochet.VM.Syntax.AST


type Program = {
  Declarations: Declaration[]
}

and Declaration =
  | DActor of Name
  | DRelation of LogicSignature<RelationPartType>
  | DDo of Statement[]

and RelationPartType =
  | RPTOne of Name
  | RPTMany of Name

and LogicSignature<'t> =
  | LSKeyword of 't * SignaturePair<'t>[]
  | LSUnary of 't * Name

and Statement =
  | SLet of Name * Expression
  | SFact of LogicSignature<Expression>
  | SForget of LogicSignature<Expression>
  | SExpression of Expression

and Expression =
  | EVariable of name:string
  | EInteger of bigint
  | EText of string
  | EActor of name:string
  | ENothing
  | ETrue

and Name = string

and SignaturePair<'t> = Name * 't

let program ds : Program = { Declarations = ds }