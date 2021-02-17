module Crochet.VM.Logic.Unification

open Crochet.VM.Runtime.Intrinsics
open Crochet.VM.Logic.IR
open Crochet.VM.Utils.Extensions

type UnificationEnvironment<'t> = Map<string, 't>

type CEvalError =
  | CEEUndefinedVariable of name:string

module UnificationEnv =
  let empty : UnificationEnvironment<'t> = Map.empty

  let bind name value (env:UnificationEnvironment<'t>) =
    match Map.tryFind name env with
    | Some x when x = value -> Some env
    | None -> Some (Map.add name value env)
    | Some _ -> None

  let define name value (env:UnificationEnvironment<'t>) =
    Map.update name value env

  let defineAll pairs (env:UnificationEnvironment<'t>) =
    pairs |> Seq.fold (fun e (k, v) -> define k v e) env

  let lookup name (env:UnificationEnvironment<'t>) =
    Map.tryFind name env

let evalPattern pattern value env =
  match (pattern, value) with
  | (PInteger x, VInteger y) when x = y -> Some env
  | (PText x, VText y) when x = y -> Some env
  | (PNothing, VNothing) -> Some env
  | (PVariable n, x) -> UnificationEnv.bind n x env
  | (PWildcard, _) -> Some env
  | _, _ -> None

let evalBinOp op l r =
  match op with
  | OpAnd -> Prim.band l r
  | OpOr -> Prim.bor l r
  | OpEqual -> Prim.eq l r
  | OpNotEqual -> Prim.neq l r
  | OpGreaterThan -> Prim.gt l r 
  | OpGreaterOrEqual -> Prim.gte l r 
  | OpLessThan -> Prim.lt l r
  | OpLessOrEqual -> Prim.lte l r

let rec evalConstraint expr env : Result<Value, CEvalError> =
  match expr with
  | CBinOp (op, l, r) ->
      Result.map2 (evalBinOp op) (evalConstraint l env) (evalConstraint r env)
  | CNot x ->
      Result.map (Prim.bnot) (evalConstraint x env)
  | CInteger x ->
      Ok (VInteger x)
  | CText x ->
      Ok (VText x)
  | CTrue ->
      Ok (VTrue)
  | CNothing ->
      Ok (VNothing)
  | CVariable name ->
      match UnificationEnv.lookup name env with
      | Some value -> Ok value
      | None -> Error (CEEUndefinedVariable name)