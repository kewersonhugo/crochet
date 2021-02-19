module Crochet.VM.Syntax.AST


type Program = {
  Declarations: Declaration[]
}

and Declaration =
  | DActor of Name
  | DRelation of LogicSignature<RelationPartType>
  | DAction of InterpolateText<Name> * Predicate * Statement[]
  | DBefore of Predicate * Statement[]
  | DAfter of Predicate * Statement[]
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
  | EActor of name:string
  | ELiteral of Literal

and Literal =
  | LInteger of bigint
  | LText of string
  | LFalse
  | LTrue

and Name = string

and Predicate = {
  Clauses: Clause[]
  Constraint: Constraint
}

and Clause = LogicSignature<LogicPattern>

and LogicPattern =
  | LPVariable of string
  | LPWildcard
  | LPActor of string
  | LPLiteral of Literal

and ConstraintOp =
  | OpAnd
  | OpOr
  | OpEq
  | OpNotEq
  | OpGte
  | OpGt
  | OpLte
  | OpLt

and Constraint =
  | CBinOp of ConstraintOp * Constraint * Constraint
  | CNot of Constraint
  | CVariable of Name
  | CLiteral of Literal

and SignaturePair<'t> = Name * 't

and InterpolateText<'t> = TextPart<'t>[]

and TextPart<'t> =
  | TPStatic of string
  | TPDynamic of 't

let program ds : Program = { Declarations = ds }

let predicate cs c : Predicate = {
  Clauses = cs
  Constraint = c
}