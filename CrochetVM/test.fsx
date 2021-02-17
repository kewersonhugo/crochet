#load "./utils/extensions.fs"
#load "./runtime/intrinsics.fs"
#load "./logic/ir.fs"
#load "./logic/unification.fs"
#load "./logic/tree.fs"
#load "./logic/database.fs"

open Crochet.VM.Utils.Extensions
open Crochet.VM.Runtime.Intrinsics
open Crochet.VM.Logic.IR
open Crochet.VM.Logic.Tree
open Crochet.VM.Logic.Unification
open Crochet.VM.Logic.Database

let db = Database.empty

let at = TMany(TOne TEnd)
Database.insertRelation "at:" at db

Database.insertValues "at:" [VText "player"; VText "foyer"] db

let atLays = TMany(TMany(TOne TEnd))
Database.insertRelation "at:lays:" atLays db

Database.insertValues "at:lays:" [VText "foyer"; VText "north"; VText "street"] db
Database.insertValues "at:lays:" [VText "foyer"; VText "south"; VText "bar"] db
Database.insertValues "at:lays:" [VText "foyer"; VText "west"; VText "cloakroom"] db
Database.insertValues "at:lays:" [VText "bar"; VText "north"; VText "foyer"] db

let reachable = {
  Parameters = ["To"; "Direction"]
  Cases =
    [
      {
        Relations = [
          ("at:", [PText "player"; PVariable "From"])
          ("at:lays:", [PVariable "From"; PVariable "Direction"; PVariable "To"])
        ]
        Constraint = CTrue
      }
    ]
}
Database.insertPredicate "reachable:" reachable db

let Pred1 = {
  Relations = [
    ("at:", [PText "player"; PVariable "From"])
    ("at:lays:", [PVariable "From"; PVariable "Direction"; PVariable "Target"])
  ]
  Constraint = CTrue
}
let Pred2 = {
  Relations = [
    ("at:lays:", [PVariable "From"; PVariable "Direction"; PVariable "Room"])
    ("reachable:", [PVariable "Room"; PText "north"])
  ]
  Constraint = CBinOp (OpEqual, CVariable "From", CVariable "From")
}
Database.searchRealise Pred2 db
