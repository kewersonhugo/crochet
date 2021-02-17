#load "./utils/extensions.fs"
#load "./runtime/intrinsics.fs"
#load "./logic/ir.fs"
#load "./logic/unification.fs"
#load "./logic/tree.fs"
#load "./logic/database.fs"

open Crochet.VM.Runtime.Intrinsics
open Crochet.VM.Logic.IR
open Crochet.VM.Logic.Tree
open Crochet.VM.Logic.Database

let db = Database.empty

let atLays = TMany(TMany(TOne TEnd))
Database.insertRelation "at:lays:" atLays db

Database.insertValues "at:lays:" [VText "foyer"; VText "north"; VText "street"] db
Database.insertValues "at:lays:" [VText "foyer"; VText "south"; VText "bar"] db
Database.insertValues "at:lays:" [VText "foyer"; VText "west"; VText "cloakroom"] db
Database.insertValues "at:lays:" [VText "bar"; VText "north"; VText "foyer"] db

let (Some [atLaysR]) = Database.lookupRelations ["at:lays:"] db
let Pred = {
  Relations = [(atLaysR, [PVariable "from"; PVariable "direction"; PVariable "what"])]
  Constraint = CBinOp (OpEqual, (CText "north"), (CVariable "direction"))
}
let (Ok res) = Database.search Pred
List.ofSeq res