module Crochet.VM.Utils.Extensions

module Map =
  let update k v map =
    if Map.containsKey k map then
      Map.add k v (Map.remove k map)
    else
      Map.add k v map

module List =
  let split list =
    match list with
    | (x :: xs) -> (x, xs)
    | _ -> failwithf "internal: empty list"

module Result =
  type ResultComputation() =
    member __.Bind (x, f) = Result.bind f x
    member __.Return (x) = Ok x
    member __.ReturnFrom (x) = x

  let result = ResultComputation()

  let map2 f x y =
    result {
      let! x = x
      let! y = y
      return f x y
    }

module Option =
  type OptionComputation() =
    member __.Bind (x, f) = Option.bind f x
    member __.Return (x) = Some x
    member __.ReturnFrom (x) = x

  let option = OptionComputation()

  let sequence xs =
    let f r x =
      option {
        let! r = r
        let! x = x
        return x :: r
      }
    Seq.fold f (Some []) xs


[<AutoOpen>]
module Computations =
  let result = Result.result
