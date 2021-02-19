module Crochet.VM.IR.AST

type Program = {
  Declarations: Declaration list
}

and Declaration =
  | DActor of Name
  | DRelation of Name * RelationType
  | DAction of InterpolateText<Name> * Predicate * Expression
  | DEvent of EventType * Predicate * Expression
  | DDo of Expression

and Expression =
  | ELet of Name * CExpression * Expression
  | ESeq of CExpression * Expression
  | EComplex of CExpression
  | EAtomic of AExpression

and CExpression =
  | CEForget of Name * AExpression list
  | CEFact of Name * AExpression list
  | CEActor of Name
  | CEAtomic of AExpression

and AExpression =
  | AEVariable of Name
  | AELiteral of Literal

and Literal =
  | LInteger of bigint
  | LText of string
  | LTrue
  | LFalse

and Predicate = {
  Clauses: Clause list
  Constraint: Constraint
}

and Clause = (string * Pattern list)

and Pattern =
  | PLiteral of Literal
  | PVariable of name:string
  | PActor of name:string
  | PWildcard

and ConstraintOp =
  | OpAnd
  | OpOr
  | OpEqual
  | OpNotEqual
  | OpGreaterThan
  | OpGreaterOrEqual
  | OpLessThan
  | OpLessOrEqual

and Constraint =
  | CNot of Constraint
  | CBinOp of ConstraintOp * Constraint * Constraint
  | CLiteral of Literal
  | CVariable of name:string

and RelationType =
  | RTOne of RelationType
  | RTMany of RelationType
  | RTEnd

and InterpolateText<'t> = TextPart<'t> list

and TextPart<'t> =
  | TPStatic of string
  | TPDynamic of 't

and EventType =
  | ETBefore
  | ETAfter

and Name = string