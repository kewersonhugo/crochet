module Crochet.VM.Logic.Database

open Crochet.VM.Utils.Extensions
open Crochet.VM.Runtime.Intrinsics
open Crochet.VM.Logic.IR
open Crochet.VM.Logic.Tree
open Crochet.VM.Logic.Unification

type Predicate = {
  Relations: (RelationTree * Pattern list) list
  Constraint: Constraint
}

type SearchError =
  | SEConstraintError of CEvalError

exception SearchErrorException of SearchError

type Database() =
  let mutable trees: Map<string, RelationTree> = Map.empty

  member internal __.Trees = trees
  member internal __.SetTrees t = trees <- t

module Predicate =
  let relations (p:Predicate) = p.Relations
  let constraints (p:Predicate) = p.Constraint

module Database =
  let empty = Database()

  let lookupRelation name (db:Database) =
    Map.tryFind name db.Trees

  let lookupRelations names (db:Database) =
    Option.sequence (List.map (fun x -> lookupRelation x db) names)
    
  let insertRelation name typ (db:Database) =
    db.SetTrees (Map.add name (Tree.empty typ) db.Trees)

  let insertValues name values (db:Database) =
    match lookupRelation name db with
    | Some tree ->
        db.SetTrees (Map.update name (Tree.insert values tree) db.Trees)
    | None ->
        failwithf "internal: invalid relation %s" name

  let removeValues name values (db:Database) =
    match lookupRelation name db with
    | Some tree ->
        match Tree.remove values tree with
        | Some tree -> db.SetTrees (Map.update name tree db.Trees)
        | None -> db.SetTrees (Map.update name (Tree.empty (Tree.getType tree)) db.Trees)
    | None ->
        failwithf "internal: invalid relation %s" name

  let rec private depthFirst relations constraints env =
    seq {
      match relations with
      | ((r, p) :: rs) ->
          for e in find p r env do
            yield! depthFirst rs constraints e
      | [] ->
          match evalConstraint constraints env with
          | Ok VNothing -> ignore None
          | Ok _ -> yield env
          | Error e -> raise (SearchErrorException (SEConstraintError e))
    }

  let search pred =
    try
      Ok (depthFirst (Predicate.relations pred) (Predicate.constraints pred) (UnificationEnv.empty))
    with
      | SearchErrorException e -> Error e


