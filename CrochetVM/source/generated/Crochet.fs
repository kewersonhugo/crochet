// This code was automatically generated from a grammar definition by Fohm.
module Fohm.Generated.Crochet

type Offset = 
  { line: int; column: int }

type OffsetRecord<'a> =
  { start: 'a; ``end``: 'a }

type Position = 
  {
    offset: unit -> OffsetRecord<int>
    position: unit -> OffsetRecord<Offset>
    sourceSlice: string
    sourceString: string
    filename: string option
  }

type Meta = 
  { source: Position; children: Position[] }

type ParseOptions =
  { filename: string option }


open Crochet.VM.Syntax.AST
open Fable.Core

[<Emit("Number($0)")>]
let parseNumber s : double = jsNative

[<Emit("BigInt($0)")>]
let parseInt s : bigint = jsNative

[<Emit("JSON.parse($0)")>]
let parseJson s : string = jsNative

let parseString (s:string) =
  parseJson ((s.Replace("\r\n", "\\n")).Replace("\n", "\\n"))

let fixNumber (s:string) = s.Replace("_", "")


open Fable.Core
open Fable.Core.JsInterop

[<Import("makeParser", from="./fohm-runtime.js")>]
let private makeParser (source: string, visitor: obj): obj = jsNative

let private visitor = 
  createObj [
    "program_alt0" ==> fun (meta:Meta) _0 ds _2 _3 ->
       program ds 
              
    "actorDeclaration_alt0" ==> fun (meta:Meta) _0 n _2 ->
       DActor n 
              
    "relationDeclaration_alt0" ==> fun (meta:Meta) _0 s _2 ->
       DRelation s 
              
    "relationPart_alt0" ==> fun (meta:Meta) n _1 ->
       RPTMany n 
              
    "relationPart_alt1" ==> fun (meta:Meta) n ->
       RPTOne n 
              
    "factStatement_alt0" ==> fun (meta:Meta) _0 f ->
       SFact f 
              
    "forgetStatement_alt0" ==> fun (meta:Meta) _0 f ->
       SForget f 
              
    "primaryExpression_alt0" ==> fun (meta:Meta) x ->
       EInteger x 
              
    "primaryExpression_alt1" ==> fun (meta:Meta) x ->
       EText x 
              
    "primaryExpression_alt2" ==> fun (meta:Meta) _0 ->
       ENothing 
              
    "primaryExpression_alt3" ==> fun (meta:Meta) _0 ->
       ETrue 
              
    "statementBlock_alt0" ==> fun (meta:Meta) _0 xs _2 _3 ->
       xs 
              
    "logicSignature_alt0" ==> fun (meta:Meta) s kws ->
       LSKeyword (s, kws) 
              
    "logicSignature_alt1" ==> fun (meta:Meta) s n ->
       LSUnary (s, n) 
              
    "logicSignaturePair_alt0" ==> fun (meta:Meta) kw p ->
       (kw, p) 
              
    "t_integer_alt0" ==> fun (meta:Meta) x ->
       parseInt (fixNumber x) 
              
    "t_float_alt0" ==> fun (meta:Meta) x ->
       parseNumber (fixNumber x) 
              
    "t_text_alt0" ==> fun (meta:Meta) x ->
       parseString x 
              
  ]

let private primParser: obj  =
  makeParser(
    """
    Crochet {
      program =
        | header declaration* space* end -- alt0
              
      
      declaration =
        | actorDeclaration -- alt0
        | relationDeclaration -- alt1
        | doDeclaration -- alt2
              
      
      actorDeclaration =
        | actor_ actorName s<";"> -- alt0
              
      
      relationDeclaration =
        | relation_ logicSignature<relationPart> ";" -- alt0
              
      
      relationPart =
        | name s<"*"> -- alt0
        | name -- alt1
              
      
      doDeclaration =
        | do_ statementBlock<statements> -- alt0
              
      
      statements =
        | factStatement -- alt0
        | forgetStatement -- alt1
              
      
      factStatement =
        | fact_ logicSignature<expression> -- alt0
              
      
      forgetStatement =
        | forget_ logicSignature<expression> -- alt0
              
      
      expression =
        | primaryExpression -- alt0
              
      
      primaryExpression =
        | integer -- alt0
        | text -- alt1
        | nothing -- alt2
        | true -- alt3
              
      
      integer =
        | t_integer -- alt0
              
      
      float =
        | t_float -- alt0
              
      
      text =
        | t_text -- alt0
              
      
      true =
        | true_ -- alt0
              
      
      nothing =
        | nothing_ -- alt0
              
      
      statementBlock<typ> =
        | s<"{"> nonemptyListOf<type, s<";">> ";"? s<"}"> -- alt0
              
      
      logicSignature<t> =
        | t logicSignaturePair<t>+ -- alt0
        | t atom -- alt1
              
      
      logicSignaturePair<t> =
        | keyword t -- alt0
              
      
      s<p> =
        | space* p -- alt0
              
      
      header (a,f,i,l,e,h,e,a,d,e,r) =
        | "%" hs* "crochet" nl -- alt0
              
      
      hs =
        | " " -- alt0
        | "\t" -- alt1
              
      
      nl =
        | "\n" -- alt0
        | "\r" -- alt1
              
      
      line =
        | (~nl any)* -- alt0
              
      
      comment (a,c,o,m,m,e,n,t) =
        | "//" line -- alt0
              
      
      space +=
        | comment -- alt0
              
      
      atom_start =
        | "a".."z" -- alt0
              
      
      atom_rest =
        | letter -- alt0
        | digit -- alt1
        | "-" -- alt2
              
      
      t_atom (a,n,a,t,o,m) =
        | atom_start atom_rest* -- alt0
              
      
      t_keyword (a,k,e,y,w,o,r,d) =
        | t_atom ":" -- alt0
              
      
      t_actor_name (a,n,a,c,t,o,r,n,a,m,e) =
        | "#" t_atom -- alt0
              
      
      name_start =
        | "A".."Z" -- alt0
        | "_" -- alt1
              
      
      name_rest =
        | letter -- alt0
        | digit -- alt1
        | "-" -- alt2
              
      
      t_name (a,n,a,m,e) =
        | name_start name_rest* -- alt0
              
      
      t_infix_symbol =
        | "+" -- alt0
        | "-" -- alt1
        | "*" -- alt2
        | "/" -- alt3
        | "<" -- alt4
        | ">" -- alt5
        | "<=" -- alt6
        | ">=" -- alt7
        | "===" -- alt8
        | "=/=" -- alt9
              
      
      dec_digit =
        | "0".."9" -- alt0
        | "_" -- alt1
              
      
      t_integer =
        | t_integer_b -- alt0
              
      
      t_integer_b (a,n,i,n,t,e,g,e,r) =
        | ~"_" dec_digit+ -- alt0
              
      
      t_float =
        | t_float_b -- alt0
              
      
      t_float_b (a,f,l,o,a,t,i,n,g,-,p,o,i,n,t,n,u,m,b,e,r) =
        | ~"_" dec_digit+ "." dec_digit+ -- alt0
              
      
      text_character =
        | "\\" "\"" -- alt0
        | ~"\"" any -- alt1
              
      
      t_text =
        | t_text_b -- alt0
              
      
      t_text_b (a,t,e,x,t) =
        | "\"" text_character* "\"" -- alt0
              
      
      kw<word> =
        | s<word> ~atom_rest -- alt0
              
      
      true_ =
        | kw<"true"> -- alt0
              
      
      false_ =
        | kw<"false"> -- alt0
              
      
      nothing_ =
        | kw<"nothing"> -- alt0
              
      
      scene_ =
        | kw<"scene"> -- alt0
              
      
      command_ =
        | kw<"command"> -- alt0
              
      
      do_ =
        | kw<"do"> -- alt0
              
      
      return_ =
        | kw<"return"> -- alt0
              
      
      goto_ =
        | kw<"goto"> -- alt0
              
      
      let_ =
        | kw<"let"> -- alt0
              
      
      end_ =
        | kw<"end"> -- alt0
              
      
      actor_ =
        | kw<"actor"> -- alt0
              
      
      relation_ =
        | kw<"relation"> -- alt0
              
      
      fact_ =
        | kw<"fact"> -- alt0
              
      
      forget_ =
        | kw<"forget"> -- alt0
              
      
      search_ =
        | kw<"search"> -- alt0
              
      
      action_ =
        | kw<"action"> -- alt0
              
      
      when_ =
        | kw<"when"> -- alt0
              
      
      choose_ =
        | kw<"choose"> -- alt0
              
      
      if_ =
        | kw<"if"> -- alt0
              
      
      and_ =
        | kw<"and"> -- alt0
              
      
      or_ =
        | kw<"or"> -- alt0
              
      
      not_ =
        | kw<"not"> -- alt0
              
      
      context_ =
        | kw<"context"> -- alt0
              
      
      trigger_ =
        | kw<"trigger"> -- alt0
              
      
      then_ =
        | kw<"then"> -- alt0
              
      
      else_ =
        | kw<"else"> -- alt0
              
      
      match_ =
        | kw<"match"> -- alt0
              
      
      repeatable_ =
        | kw<"repeatable"> -- alt0
              
      
      reserved =
        | true_ -- alt0
        | false_ -- alt1
        | nothing_ -- alt2
        | scene_ -- alt3
        | command_ -- alt4
        | do_ -- alt5
        | return_ -- alt6
        | goto_ -- alt7
        | let_ -- alt8
        | end_ -- alt9
        | actor_ -- alt10
        | relation_ -- alt11
        | fact_ -- alt12
        | search_ -- alt13
        | forget_ -- alt14
        | action_ -- alt15
        | when_ -- alt16
        | choose_ -- alt17
        | if_ -- alt18
        | and_ -- alt19
        | or_ -- alt20
        | not_ -- alt21
        | context_ -- alt22
        | trigger_ -- alt23
        | then_ -- alt24
        | else_ -- alt25
        | match_ -- alt26
        | repeatable_ -- alt27
              
    }
      
    """, 
    visitor
  )

let parse (rule: string) (source: string) (options: ParseOptions): Result<Program, string> = 
  let (success, value) = !!(primParser$(source, rule, options))
  if success then Ok(!!value)
  else Error(!!value)
  