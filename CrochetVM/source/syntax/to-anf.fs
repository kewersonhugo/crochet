module Crochet.VM.Syntax.ToANF

module IR = Crochet.VM.IR.AST
open Crochet.VM.Syntax.AST
open Crochet.VM.Utils.Continuation

type IdBox() =
  let mutable nextId = 1

  member __.NextId() =
    let id = nextId
    nextId <- nextId + 1
    id

type Meta() =
  let nameId = IdBox()

  member __.NextNameId() = nameId.NextId()

module Meta =
  let empty = Meta()

  let fresh prefix (m: Meta) =
    let id = m.NextNameId()
    sprintf "%s%d" ("%" + prefix) id


let signatureToName signature =
  match signature with
  | LSKeyword (_, kws) ->
      kws |> Seq.map fst |> String.concat ""
  | LSUnary (_, name) ->
      name

let signatureToValues signature =
  match signature with
  | LSKeyword (s, kws) ->
      s :: (Seq.map snd kws |> List.ofSeq)
  | LSUnary (s, _) ->
      [s]

let signatureToPair signature =
  (signatureToName signature, signatureToValues signature)

let lowerRelationType typ =
  match typ with
  | RPTOne _ -> IR.RTOne
  | RPTMany _ -> IR.RTMany

let rec normaliseName f m expr k =
  let rec go value =
    match value with
    | IR.EAtomic e -> k e
    | IR.EComplex e ->
        let name = Meta.fresh "r" m
        IR.ELet(name, e, k (IR.AEVariable name))
    | IR.ELet (name, value, body) ->
        IR.ELet(name, value, go body)
    | IR.ESeq (value, body) ->
        IR.ESeq(value, go body)
  in f m expr go

and normaliseNames f meta exprs k =
  let rec go exprs k =
    match exprs with
    | [] -> k []
    | x :: xs ->
        normaliseName f meta x (fun x' -> go xs (fun xs' -> k (x' :: xs')))
  in go (Seq.toList exprs) k

let getComplex expr =
  match expr with
  | IR.EComplex x -> x
  | IR.EAtomic x -> IR.CEAtomic x
  | _ -> failwithf "internal: expected complex expression"

let rec lowerExpression meta expr =
  cont {
    match expr with
    | EVariable x ->
        return IR.EAtomic(IR.AEVariable x)
    | EInteger i ->
        return IR.EAtomic(IR.AELiteral(IR.LInteger i))
    | EText t ->
        return IR.EAtomic(IR.AELiteral(IR.LText t))
    | ENothing ->
        return IR.EAtomic(IR.AELiteral(IR.LFalse))
    | ETrue ->
        return IR.EAtomic(IR.AELiteral(IR.LTrue))
    | EActor name ->
        return IR.EComplex(IR.CEActor name)
  }

let rec lowerStatement meta stmt =
  cont {
    match stmt with
    // let a = b; x... is handled by lowerStatements
    | SLet (_, expr) ->
        return! lowerExpression meta expr
    | SFact signature ->
        let (name, args) = signatureToPair signature
        let! args = normaliseNames lowerExpression meta args
        return IR.EComplex(IR.CEFact(name, args))
    | SForget signature ->
        let (name, args) = signatureToPair signature
        let! args = normaliseNames lowerExpression meta args
        return IR.EComplex(IR.CEForget(name, args))
    | SExpression expr ->
        return! lowerExpression meta expr
  }

let rec lowerStatements meta stmts =
  cont {
    match stmts with
    | ([stmt]) ->
        return! lowerStatement meta stmt
    | (SLet (name, expr) :: rest) ->
        let! expr = lowerExpression meta expr $ getComplex
        let rest = lowerStatements meta rest id
        return IR.ELet(name, expr, rest)
    | (stmt :: rest) ->
        let! stmt = lowerStatement meta stmt
        let rest = lowerStatements meta rest id
        return IR.ESeq(getComplex stmt, rest)
    | [] ->
        return IR.EAtomic(IR.AELiteral(IR.LFalse))
  }

let rec lowerDeclaration meta decl =
  match decl with
  | DActor name -> [IR.DActor name]
  | DRelation s ->
      let name = signatureToName s
      let types = signatureToValues s |> List.map lowerRelationType
      [IR.DRelation (name, types)]
  | DDo stmts ->
      [IR.DDo (lowerStatements meta (List.ofSeq stmts) id)]

let lowerProgram (prog:Program) : IR.Program =
  let meta = Meta.empty
  {
    Declarations = Seq.collect (lowerDeclaration meta) prog.Declarations
                   |> List.ofSeq
  }