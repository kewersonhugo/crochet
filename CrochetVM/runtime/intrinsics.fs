module Crochet.VM.Runtime.Intrinsics

type Value =
  | VInteger of int
  | VText of string
  | VTrue
  | VNothing

module Prim =
  let bor a b =
    match a, b with
    | VNothing, VNothing -> VNothing
    | VNothing, result -> result
    | result, _ -> result

  let band a b =
    match a, b with
    | VNothing, _ -> VNothing
    | _, VNothing -> VNothing
    | _, result -> result

  let bnot a =
    match a with
    | VNothing -> VTrue
    | _ -> VNothing

  let lt a b =
    match a, b with
    | VInteger a, VInteger b when a < b -> VInteger b
    | VInteger _, VInteger _ -> VNothing
    | _, _ -> failwithf "internal: invalid types"

  let lte a b =
    match a, b with
    | VInteger a, VInteger b when a <= b -> VInteger b
    | VInteger _, VInteger _ -> VNothing
    | _, _ -> failwithf "internal: invalid types"

  let gt a b =
    match a, b with
    | VInteger a, VInteger b when a > b -> VInteger b
    | VInteger _, VInteger _ -> VNothing
    | _, _ -> failwithf "internal: invalid types"

  let gte a b =
    match a, b with
    | VInteger a, VInteger b when a >= b -> VInteger b
    | VInteger _, VInteger _ -> VNothing
    | _, _ -> failwithf "internal: invalid types"

  let eq a b =
    if a = b then b else VNothing
  
  let neq a b =
    if a <> b then b else VNothing

  