module Crochet.VM.Utils.Continuation

module Continuation =
  let ret x = fun k -> k x
  let bind m f = fun k -> m (fun i -> f i k)
  let map m f = fun k -> m (fun i -> k (f i))

  type MCont() =
    member __.Bind(m, f) = bind m f
    member __.Return x = ret x
    member __.ReturnFrom x = x

let (>>=) = Continuation.bind
let ($) = Continuation.map
let cont = Continuation.MCont()