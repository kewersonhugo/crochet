module Crochet.VM.Logic.Database

open Crochet.VM.Utils.Extensions
open Crochet.VM.Runtime.Intrinsics
open Crochet.VM.Logic.IR
open Crochet.VM.Logic.Tree
open Crochet.VM.Logic.Unification

type Predicate = {
  Relations: (string * Pattern list) list
  Constraint: Constraint
}

type PredicateRelation = {
  Parameters: string list
  Cases: Predicate list
}

type Relation =
  | RTree of RelationTree
  | RPredicate of PredicateRelation

type SearchError =
  | SEConstraintError of CEvalError
  | SEUndefinedRelation of string

exception SearchErrorException of SearchError

type Database() =
  let mutable predicates: Map<string, PredicateRelation> = Map.empty
  let mutable trees: Map<string, RelationTree> = Map.empty

  member internal __.Trees = trees
  member internal __.SetTrees t = trees <- t

  member internal __.Predicates = predicates
  member internal __.SetPredicates p = predicates <- p

module Predicate =
  let relations (p:Predicate) = p.Relations
  let constraints (p:Predicate) = p.Constraint

module PredicateRelation =
  let parameters (p:PredicateRelation) = p.Parameters
  let cases (p:PredicateRelation) = p.Cases

module Database =
  let empty = Database()

  let lookupRelation name (db:Database) =
    Map.tryFind name db.Trees

  let insertRelation name typ (db:Database) =
    db.SetTrees (Map.add name (Tree.empty typ) db.Trees)
  
  let lookupPredicate name (db:Database) =
    Map.tryFind name db.Predicates

  let insertPredicate name pred (db:Database) =
    db.SetPredicates (Map.add name pred db.Predicates)

  let lookup name (db:Database) =
    match lookupRelation name db with
    | Some x -> Some (RTree x)
    | None ->
        match lookupPredicate name db with
        | Some x -> Some (RPredicate x)
        | None -> None

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

  let rec private findFirstPredicate patterns preds env db =
    match preds with
    | (p :: ps) ->
        let relations = Predicate.relations p
        let constraints = Predicate.constraints p
        let resultEnv = depthFirst relations constraints UnificationEnv.empty db
        if Seq.isEmpty resultEnv then
          findFirstPredicate patterns ps env db
        else
          resultEnv
    | [] -> Seq.empty

  and private findPredicate patterns pred env db =
    let join e (k, p) =
      printfn "join %A (%A, %A) %A" e k p env
      match e with
      | None -> None
      | Some (env, e) ->
          match (UnificationEnv.lookup k e) with
          | Some v -> Option.map (fun env -> env, e) (evalPattern p v env)
          | None -> None

    let cases = PredicateRelation.cases pred
    let parameters = PredicateRelation.parameters pred
    let bindings = List.zip parameters patterns
    let resultEnv = findFirstPredicate patterns cases env db
    printfn "Result: %A - %A - %A" env bindings (List.ofSeq resultEnv)
    seq {
      for e in resultEnv do
        match List.fold join (Some (env, e)) bindings with
        | Some (e, _) -> yield e
        | None -> ignore None
    }

  and private find patterns relation env db =
    match relation with
    | RTree t -> Tree.find patterns t env
    | RPredicate p -> findPredicate patterns p env db

  and private depthFirst relations constraints env db =
    seq {
      match relations with
      | ((name, patterns) :: rs) ->
          match lookup name db with
          | Some relation ->
              for e in find patterns relation env db do
                yield! depthFirst rs constraints e db
          | None ->
              printfn "Undefined relation %s" name
              raise (SearchErrorException (SEUndefinedRelation name))
      | [] ->
          match evalConstraint constraints env with
          | Ok VNothing -> ignore None
          | Ok _ -> yield env
          | Error e ->
              printfn "Constraint error %A" e
              raise (SearchErrorException (SEConstraintError e))
    }

  let search pred (db:Database) =
    depthFirst (Predicate.relations pred) (Predicate.constraints pred) (UnificationEnv.empty) db

  let searchRealise pred db =
    try Ok (List.ofSeq (search pred db))
    with
      SearchErrorException e -> Error e


