module Crochet.VM.Syntax.Parser

open Fohm.Generated
open Crochet.VM.Syntax.AST

let parse source filename =
  Crochet.parse "Program" source { filename = filename }
