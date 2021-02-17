module Crochet.VM.IR.AST

type Program = {
  Declarations: Declaration list
}

and Declaration =
  | DActor of Name
  | DRelation of Name * RelationType list
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

and RelationType =
  | RTOne
  | RTMany

and Name = string