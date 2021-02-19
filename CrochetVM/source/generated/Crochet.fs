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


open Fable.Core
open Crochet.VM.Syntax.AST

[<Emit("Number($0)")>]
let parseNumber s : double = jsNative

[<Emit("BigInt($0)")>]
let parseInt s : bigint = jsNative

[<Emit("JSON.parse($0)")>]
let parseJson s : string = jsNative

let parseString (s:string) =
  parseJson ((s.Replace("\r\n", "\\n")).Replace("\n", "\\n"))

let escaped (s:string) =
  parseString ("\"" + s.Replace("\"", "\\\"") + "\"")

let fixNumber (s:string) = s.Replace("_", "")


open Fable.Core
open Fable.Core.JsInterop

[<Import("makeParser", from="./fohm-runtime.js")>]
let private makeParser (source: string, visitor: obj): obj = jsNative

let private visitor = 
  createObj [
    "program_alt0" ==> fun (meta:Meta) _0 ds _2 _3 ->
       program ds 
              
    "declarations_alt0" ==> fun (meta:Meta) xs _1 ->
       xs 
              
    "actorDeclaration_alt0" ==> fun (meta:Meta) _0 n ->
       DActor n 
              
    "relationDeclaration_alt0" ==> fun (meta:Meta) _0 s ->
       DRelation s 
              
    "relationPart_alt0" ==> fun (meta:Meta) n _1 ->
       RPTMany n 
              
    "relationPart_alt1" ==> fun (meta:Meta) n ->
       RPTOne n 
              
    "actionDeclaration_alt0" ==> fun (meta:Meta) _0 t _2 p b ->
       DAction(t, p, b) 
              
    "beforeDeclaration_alt0" ==> fun (meta:Meta) _0 p b ->
       DBefore(p, b) 
              
    "afterDeclaration_alt0" ==> fun (meta:Meta) _0 p b ->
       DAfter(p, b) 
              
    "doDeclaration_alt0" ==> fun (meta:Meta) _0 xs ->
       DDo xs 
              
    "predicate_alt0" ==> fun (meta:Meta) cs _1 c ->
       predicate cs c 
              
    "predicate_alt1" ==> fun (meta:Meta) cs ->
       predicate cs (CLiteral LTrue) 
              
    "constraint_alt0" ==> fun (meta:Meta) l op r ->
       CBinOp (op, l, r) 
              
    "constraint_alt1" ==> fun (meta:Meta) _0 l ->
       CNot l 
              
    "constraint_alt2" ==> fun (meta:Meta) n ->
       CVariable n 
              
    "constraint_alt3" ==> fun (meta:Meta) l ->
       CLiteral l 
              
    "constraint_op_alt0" ==> fun (meta:Meta) _0 ->
       OpAnd 
              
    "constraint_op_alt1" ==> fun (meta:Meta) _0 ->
       OpOr 
              
    "constraint_op_alt2" ==> fun (meta:Meta) _0 ->
       OpEq 
              
    "constraint_op_alt3" ==> fun (meta:Meta) _0 ->
       OpNotEq 
              
    "constraint_op_alt4" ==> fun (meta:Meta) _0 ->
       OpGt 
              
    "constraint_op_alt5" ==> fun (meta:Meta) _0 ->
       OpGte 
              
    "constraint_op_alt6" ==> fun (meta:Meta) _0 ->
       OpLt 
              
    "constraint_op_alt7" ==> fun (meta:Meta) _0 ->
       OpLte 
              
    "statement_alt1" ==> fun (meta:Meta) e ->
       SExpression e 
              
    "letStatement_alt0" ==> fun (meta:Meta) _0 n _2 e ->
       SLet (n, e) 
              
    "factStatement_alt0" ==> fun (meta:Meta) _0 f ->
       SFact f 
              
    "forgetStatement_alt0" ==> fun (meta:Meta) _0 f ->
       SForget f 
              
    "primaryExpression_alt0" ==> fun (meta:Meta) n ->
       EActor n 
              
    "primaryExpression_alt1" ==> fun (meta:Meta) l ->
       ELiteral l 
              
    "primaryExpression_alt2" ==> fun (meta:Meta) _0 e _2 ->
       e 
              
    "integer_alt0" ==> fun (meta:Meta) x ->
       LInteger x 
              
    "text_alt0" ==> fun (meta:Meta) x ->
       LText x 
              
    "true_alt0" ==> fun (meta:Meta) _0 ->
       LTrue 
              
    "nothing_alt0" ==> fun (meta:Meta) _0 ->
       LFalse 
              
    "statementBlock_alt0" ==> fun (meta:Meta) _0 xs _2 _3 ->
       xs 
              
    "logicSignature_alt0" ==> fun (meta:Meta) s kws ->
       LSKeyword (s, kws) 
              
    "logicSignature_alt1" ==> fun (meta:Meta) s n ->
       LSUnary (s, n) 
              
    "logicSignaturePair_alt0" ==> fun (meta:Meta) kw p ->
       (kw, p) 
              
    "interpolateTextPart_alt0" ==> fun (meta:Meta) _0 x ->
       TPStatic (escaped x) 
              
    "interpolateTextPart_alt1" ==> fun (meta:Meta) _0 x _2 ->
       TPDynamic x 
              
    "interpolateTextPart_alt2" ==> fun (meta:Meta) x ->
       TPStatic x 
              
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
        | header declarations space* end -- alt0
              
      
      declarations =
        | listOf<declaration, s<";">> s<";">? -- alt0
              
      
      declaration =
        | actorDeclaration -- alt0
        | relationDeclaration -- alt1
        | actionDeclaration -- alt2
        | beforeDeclaration -- alt3
        | afterDeclaration -- alt4
        | doDeclaration -- alt5
              
      
      actorDeclaration =
        | actor_ actorName -- alt0
              
      
      relationDeclaration =
        | relation_ logicSignature<relationPart> -- alt0
              
      
      relationPart =
        | name s<"*"> -- alt0
        | name -- alt1
              
      
      actionDeclaration =
        | action_ interpolateText<name> when_ predicate statementBlock<logicStmt> -- alt0
              
      
      beforeDeclaration =
        | before_ predicate statementBlock<logicStmt> -- alt0
              
      
      afterDeclaration =
        | after_ predicate statementBlock<logicStmt> -- alt0
              
      
      doDeclaration =
        | do_ statementBlock<logicStmt> -- alt0
              
      
      predicate =
        | nonemptyLisOf<clause, s<",">> if_ constraint -- alt0
        | nonemptyListOf<clause, s<",">> -- alt1
              
      
      constraint =
        | constraint constraintOp constraint -- alt0
        | not_ constraint -- alt1
        | name -- alt2
        | literal -- alt3
              
      
      constraint_op =
        | and_ -- alt0
        | or_ -- alt1
        | s<"==="> -- alt2
        | s<"=/="> -- alt3
        | s<">"> -- alt4
        | s<">="> -- alt5
        | s<"<"> -- alt6
        | s<"<="> -- alt7
              
      
      statement =
        | letStatement -- alt0
        | expression -- alt1
              
      
      logicStmt =
        | factStatement -- alt0
        | forgetStatement -- alt1
        | statement -- alt2
              
      
      letStatement =
        | let_ name s<"="> expression -- alt0
              
      
      factStatement =
        | fact_ logicSignature<expression> -- alt0
              
      
      forgetStatement =
        | forget_ logicSignature<expression> -- alt0
              
      
      expression =
        | primaryExpression -- alt0
              
      
      primaryExpression =
        | actor_name -- alt0
        | literal -- alt1
        | "(" expression ")" -- alt2
              
      
      literal =
        | integer -- alt0
        | text -- alt1
        | nothing -- alt2
        | true -- alt3
              
      
      integer =
        | t_integer -- alt0
              
      
      text =
        | t_text -- alt0
              
      
      true =
        | true_ -- alt0
              
      
      nothing =
        | nothing_ -- alt0
              
      
      actor_name =
        | t_actor_name -- alt0
              
      
      statementBlock<typ> =
        | s<"{"> listOf<type, s<";">> s<";">? s<"}"> -- alt0
              
      
      logicSignature<t> =
        | t logicSignaturePair<t>+ -- alt0
        | t atom -- alt1
              
      
      logicSignaturePair<t> =
        | keyword t -- alt0
              
      
      s<p> =
        | space* p -- alt0
              
      
      interpolateTextPart<p> =
        | "\\" any -- alt0
        | "[" s<p> s<"]"> -- alt1
        | ~"\"" any -- alt2
              
      
      interpolateText<p> (a,t,e,x,t,w,i,t,h,i,n,t,e,r,p,o,l,a,t,i,o,n) =
        | s<"\""> interpolateTextPart<p>* "\"" -- alt0
              
      
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
  