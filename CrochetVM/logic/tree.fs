module Crochet.VM.Logic.Tree

open Crochet.VM.Logic.IR
open Crochet.VM.Logic.Unification
open Crochet.VM.Utils.Extensions
open Crochet.VM.Runtime.Intrinsics

type RelationTree =
  | RTOne of RelationType * (Value * RelationTree) option
  | RTMany of RelationType * Map<Value, RelationTree>
  | RTEnd

let empty treeType =
  match treeType with
  | TOne t -> RTOne (t, None)
  | TMany t -> RTMany (t, Map.empty)
  | TEnd -> RTEnd

let rec insert fact tree =
  match (fact, tree) with
  | (x :: xs), RTOne (t, Some (v, st)) ->
      RTOne (t, Some (x, insert xs st))
  | (x :: xs), RTOne (t, None) ->
      RTOne (t, Some (x, insert xs (empty t)))
  | (x :: xs), RTMany (t, map) ->
      match Map.tryFind x map with
      | Some st -> RTMany (t, Map.update x (insert xs st) map)
      | None -> RTMany (t, Map.add x (insert xs (empty t)) map)
  | [], RTEnd ->
      RTEnd
  | _, _ ->
      failwithf "internal: invalid facts %A for tree %A" fact tree

let rec remove fact tree =
  match (fact, tree) with
  | (x :: xs), RTOne (t, Some (v, st)) ->
      if x = v then
        match remove xs st with
        | None -> None
        | Some st -> Some (RTOne (t, Some (v, st)))
      else
        Some (RTOne (t, Some (v, st)))
  | (_ :: _), RTOne (t, None) ->
      None
  | (x :: xs), RTMany (t, map) ->
      match Map.tryFind x map with
      | Some st ->
          match remove xs st with
          | None ->
              let map = Map.remove x map
              if Map.isEmpty map then None
              else Some (RTMany (t, map))
          | Some st ->
              Some (RTMany (t, Map.update x st map))
      | None ->
          Some (RTMany (t, map))
  | [], RTEnd ->
      None
  | _, _ ->
      failwithf "internal: invalid facts %A for tree %A" fact tree

let rec find patterns tree env =
  seq {
    match patterns, tree with
    | (p :: ps), RTOne (_, Some (x, st)) ->
        match evalPattern p x env with
        | Some env -> yield! find ps st env 
        | None -> ignore None
    | (_ :: _), RTOne (_, None) ->
        ignore None
    | (p :: ps), RTMany (_, map) ->
        for pair in map do
          let (k, st) = (pair.Key, pair.Value)
          match evalPattern p k env with
          | Some env -> yield! find ps st env
          | None -> ignore None
    | [], RTEnd ->
        yield env
    | _, _ ->
        failwithf "internal: invalid patterns %A for tree %A" patterns tree
  }

let getType tree =
  match tree with
  | RTOne (t, _) -> TOne t
  | RTMany (t, _) -> TMany t
  | RTEnd -> TEnd