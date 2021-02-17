module Crochet.VM.App

open Crochet.VM

let parse = Crochet.VM.Syntax.Parser.parse
let compileToIR = Crochet.VM.Syntax.ToANF.lowerProgram
