module Crochet.VM.Logic.IR

type Pattern =
  | PInteger of int
  | PText of string
  | PNothing
  | PVariable of name:string
  | PWildcard

type ConstraintOp =
  | OpAnd
  | OpOr
  | OpEqual
  | OpNotEqual
  | OpGreaterThan
  | OpGreaterOrEqual
  | OpLessThan
  | OpLessOrEqual

type Constraint =
  | CNot of Constraint
  | CBinOp of ConstraintOp * Constraint * Constraint
  | CInteger of int
  | CText of string
  | CTrue
  | CNothing
  | CVariable of name:string

type RelationType =
  | RTOne of RelationType
  | RTMany of RelationType
  | RTEnd