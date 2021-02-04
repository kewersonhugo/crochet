import * as Ohm from "ohm-js";
import * as Fs from "fs";
import * as Path from "path";
import {
  DDo,
  DFFICommand,
  DLocalCommand,
  DScene,
  EBoolean,
  EFloat,
  EInteger,
  EInvoke,
  ELet,
  ENothing,
  EText,
  EVariable,
  Expression,
  Program,
  SExpression,
  SGoto,
  Signature,
  SReturn,
  UseSignature,
} from "./ast";

const grammarFile = Path.join(__dirname, "../../../grammar/crochet.ohm");
const grammar = Ohm.grammar(Fs.readFileSync(grammarFile, "utf-8"));

export function parse(filename: string, source: string): Program {
  const match = grammar.match(source);
  if (match.failed()) {
    throw new Error(match.message);
  } else {
    const declarations = toAST(match).toAST();
    return new Program(filename, declarations);
  }
}

type x = unknown;
type Node = Ohm.Node;

class AtomSegment {
  constructor(readonly name: string) {}

  toStaticPart() {
    return this.name;
  }
}

class VariableSegment {
  constructor(readonly name: string) {}

  toStaticPart() {
    return "_";
  }
}

class ExprSegment {
  constructor(readonly expr: Expression) {}

  toStaticPart() {
    return "_";
  }
}

const toAST = grammar.createSemantics().addOperation("toAST()", {
  Program(_header: x, decls: Node, _eof: x) {
    return decls.toAST();
  },

  DoDeclaration(_do: x, body: Node) {
    return new DDo(body.toAST());
  },

  SceneDeclaration(_scene: x, name: Node, body: Node) {
    return new DScene(name.toAST(), body.toAST());
  },

  CommandDeclaration_ffi(
    _command: x,
    sig: Node,
    _eq: x,
    name: Node,
    _1: x,
    params: Node,
    _2: x,
    _semi: x
  ) {
    return new DFFICommand(sig.toAST(), name.toAST(), params.toAST());
  },

  CommandDeclaration_local(_command: x, sig: Node, body: Node) {
    return new DLocalCommand(sig.toAST(), body.toAST());
  },

  CommandSignature_single(head: Node) {
    return new Signature(head.toAST(), []);
  },

  CommandSignature_multi(head0: Node, segments0: Node) {
    const head: AtomSegment | VariableSegment = head0.toAST();
    const segments: (AtomSegment | VariableSegment)[] = [
      head,
      ...segments0.toAST(),
    ];
    const name = segments.map((x) => x.toStaticPart());
    const params = segments
      .filter((x) => x instanceof VariableSegment)
      .map((x) => x.name);
    return new Signature(name.join(" "), params);
  },

  CommandSignature_infix(left: Node, symbol: Node, right: Node) {
    return new Signature(`_ ${symbol.toAST()} _`, [
      left.toAST(),
      right.toAST(),
    ]);
  },

  SignatureSegment_static(name: Node) {
    return new AtomSegment(name.toAST());
  },

  SignatureSegment_variable(name: Node) {
    return new VariableSegment(name.toAST());
  },

  Statement_expr(expr: Node, _semi: x) {
    return new SExpression(expr.toAST());
  },

  ReturnStatement_with_value(_return: x, expr: Node, _semi: x) {
    return new SReturn(expr.toAST());
  },

  ReturnStatement_naked(_return: x, _semi: x) {
    return new SReturn(new ENothing());
  },

  GotoStatement(_goto: x, scene: Node, _semi: x) {
    return new SGoto(scene.toAST());
  },

  LetStatement(_let: x, name: Node, _eq: x, expr: Node, _semi: x) {
    return new ELet(name.toAST(), expr.toAST());
  },

  StatementBlock(_l: x, stmts: Node, _r: x) {
    return stmts.toAST();
  },

  UseInfix_infix(left: Node, symbol: Node, right: Node) {
    const sig = new UseSignature(`_ ${symbol.toAST()} _`, [
      left.toAST(),
      right.toAST(),
    ]);
    return new EInvoke(sig);
  },

  UseMixfix_multi(head0: Node, segments0: Node) {
    const head: AtomSegment | ExprSegment = head0.toAST();
    const segments: (AtomSegment | ExprSegment)[] = [
      head,
      ...segments0.toAST(),
    ];
    const name = segments.map((x) => x.toStaticPart());
    const args = segments
      .filter((x) => x instanceof ExprSegment)
      .map((x) => ((x as any) as ExprSegment).expr);
    const sig = new UseSignature(name.join(" "), args);
    return new EInvoke(sig);
  },

  UseSegment_static(name: Node) {
    return new AtomSegment(name.toAST());
  },

  UseSegment_variable(expr: Node) {
    return new ExprSegment(expr.toAST());
  },

  PrimaryExpression_atom(head: Node) {
    return new EInvoke(new UseSignature(head.toAST(), []));
  },

  PrimaryExpression_variable(name: Node) {
    return new EVariable(name.toAST());
  },

  PrimaryExpression_group(_l: x, expr: Node, _r: x) {
    return expr.toAST();
  },

  Text(node: Node) {
    return new EText(node.toAST());
  },

  Integer(node: Node) {
    return new EInteger(node.toAST());
  },

  Float(node: Node) {
    return new EFloat(node.toAST());
  },

  Boolean(node: Node) {
    return new EBoolean(node.toAST());
  },

  Nothing(_: x) {
    return new ENothing();
  },

  Name(node: Node) {
    return node.toAST();
  },

  Atom(node: Node) {
    return node.toAST();
  },

  EmptyListOf() {
    return [];
  },

  NonemptyListOf(head: Node, _sep: x, rest: Node) {
    return [head.toAST(), ...rest.toAST()];
  },

  _terminal() {
    return this.primitiveValue;
  },

  header(_1: x, _2: x, _3: x, _4: x) {
    return { language: "en", version: 1 };
  },

  text(_l: x, chars: x, _r: x) {
    return JSON.parse(this.sourceString as any);
  },

  boolean(node: Node) {
    return node.toAST();
  },

  boolean_true(_: x) {
    return true;
  },

  boolean_false(_: x) {
    return false;
  },

  integer(x: Node) {
    return BigInt(this.sourceString as any);
  },

  float(_1: x, _2: x, _3: x) {
    return Number(this.sourceString as any);
  },

  atom(_1: x, _2: x) {
    return this.sourceString;
  },

  name(_1: x, _2: x) {
    return this.sourceString;
  },
});
