import * as IR from "../../ir/operations";

type Name = string;

export class Signature {
  constructor(readonly name: Name, readonly parameters: Name[]) {}
}

export class UseSignature {
  constructor(readonly name: Name, readonly args: Expression[]) {}
}

export abstract class Node {}

export class Program extends Node {
  constructor(readonly filename: string, readonly declarations: Declaration[]) {
    super();
  }

  compile() {
    return new IR.Module(this.filename, [...this._compile()]);
  }

  private *_compile() {
    for (const declaration of this.declarations) {
      yield* declaration.compile();
    }
  }
}

//== Declaration
export abstract class Declaration extends Node {
  abstract compile(): Generator<IR.Declaration>;
}

export class DScene extends Declaration {
  constructor(readonly name: Name, readonly body: Statement[]) {
    super();
  }

  *compile() {
    yield new IR.DefineScene(
      this.name,
      to_list(
        this.body.map((x) => x.compile()),
        [new IR.Halt()]
      )
    );
  }
}

export class DDo extends Declaration {
  constructor(readonly body: Statement[]) {
    super();
  }

  *compile() {
    yield new IR.Do(
      to_list(
        this.body.map((x) => x.compile()),
        [new IR.Yield()]
      )
    );
  }
}

export class DFFICommand extends Declaration {
  constructor(
    readonly signature: Signature,
    readonly name: Name,
    readonly args: Name[]
  ) {
    super();
  }

  *compile() {
    const mapping = new Map(this.signature.parameters.map((x, i) => [x, i]));
    const args = this.args.map((x) => mapping.get(x) as number);
    yield new IR.DefineForeignCommand(
      this.signature.name,
      this.signature.parameters,
      this.name,
      args
    );
  }
}

export class DLocalCommand extends Declaration {
  constructor(readonly signature: Signature, readonly body: Statement[]) {
    super();
  }

  *compile() {
    yield new IR.DefineCommand(
      this.signature.name,
      this.signature.parameters,
      to_list(
        this.body.map((x) => x.compile()),
        [new IR.PushNothing(), new IR.Return()]
      )
    );
  }
}

export class DActor extends Declaration {
  constructor(
    readonly name: string,
    readonly roles: string[],
    readonly initialisation: SFact[]
  ) {
    super();
  }

  *compile() {
    yield new IR.DefineActor(this.name, this.roles);
    yield* new DDo(this.initialisation).compile();
  }
}

export class DAction extends Declaration {
  constructor(
    readonly repeatable: boolean,
    readonly title: EInterpolateText,
    readonly tags: string[],
    readonly predicate: IR.Predicate,
    readonly body: Statement[]
  ) {
    super();
  }

  *compile() {
    yield new IR.DefineAction(
      this.repeatable,
      this.title.compile_simple(),
      this.tags,
      this.predicate,
      to_list(
        this.body.map((x) => x.compile()),
        [new IR.Yield()]
      )
    );
  }
}

export class DContext extends Declaration {
  constructor(readonly name: string, readonly hooks: Hook[]) {
    super();
  }

  *compile() {
    yield new IR.DefineContext(
      this.name,
      this.hooks.map((x) => x.compile())
    );
  }
}

export class Hook {
  constructor(readonly predicate: IR.Predicate, readonly body: Statement[]) {}

  compile() {
    return new IR.HookDefinition(
      this.predicate,
      to_list(
        this.body.map((x) => x.compile()),
        [new IR.Yield()]
      )
    );
  }
}

export class DRelation extends Declaration {
  constructor(readonly signature: RelationSignature) {
    super();
  }

  *compile() {
    yield new IR.DefineRelation(
      this.signature.name,
      this.signature.components.map((x) => x.compile())
    );
  }
}

export class RelationSignature {
  constructor(
    readonly name: string,
    readonly components: RelationComponent[]
  ) {}
}

export abstract class RelationComponent {
  abstract compile(): IR.RelationComponent;
}
export class OneComponent {
  constructor(readonly name: string) {}
  compile() {
    return new IR.OneRelation(this.name);
  }
}
export class ManyComponent {
  constructor(readonly name: string) {}
  compile() {
    return new IR.ManyRelation(this.name);
  }
}

//== Statement
export abstract class Statement extends Node {
  abstract compile(): Generator<IR.Operation>;
}

export class SExpression extends Statement {
  constructor(readonly expr: Expression) {
    super();
  }

  *compile() {
    yield* this.expr.compile();
    yield new IR.Drop();
  }
}

export class SGoto extends Statement {
  constructor(readonly name: string) {
    super();
  }

  *compile() {
    yield new IR.Goto(this.name);
  }
}

export class SReturn extends Statement {
  constructor(readonly value: Expression) {
    super();
  }

  *compile() {
    yield* this.value.compile();
    yield new IR.Return();
  }
}

export class FactSignature {
  constructor(readonly name: string, readonly values: Expression[]) {}
}

export class SFact extends Statement {
  constructor(readonly sig: FactSignature) {
    super();
  }

  *compile() {
    for (const value of this.sig.values) {
      yield* value.compile();
    }
    yield new IR.InsertFact(this.sig.name, this.sig.values.length);
  }
}

export class SForget extends Statement {
  constructor(readonly sig: FactSignature) {
    super();
  }

  *compile() {
    for (const value of this.sig.values) {
      yield* value.compile();
    }
    yield new IR.RemoveFact(this.sig.name, this.sig.values.length);
  }
}

export class STriggerAction extends Statement {
  *compile() {
    yield new IR.TriggerAction();
  }
}

export class STrigger extends Statement {
  constructor(readonly name: string) {
    super();
  }

  *compile() {
    yield new IR.TriggerContext(this.name);
  }
}

//== Expression
export abstract class Expression {
  abstract compile(): Generator<IR.Operation>;
}

export class EInvoke extends Statement {
  constructor(readonly command: UseSignature) {
    super();
  }

  *compile() {
    for (const arg of this.command.args) {
      yield* arg.compile();
    }
    yield new IR.Invoke(this.command.name, this.command.args.length);
  }
}

export class EText extends Expression {
  constructor(readonly value: string) {
    super();
  }

  *compile() {
    yield new IR.PushText(this.value);
  }
}

export class EInteger extends Expression {
  constructor(readonly value: bigint) {
    super();
  }

  *compile() {
    yield new IR.PushInteger(this.value);
  }
}

export class EFloat extends Expression {
  constructor(readonly value: number) {
    super();
  }

  *compile() {
    yield new IR.PushFloat(this.value);
  }
}

export class EBoolean extends Expression {
  constructor(readonly value: boolean) {
    super();
  }

  *compile() {
    yield new IR.PushBoolean(this.value);
  }
}

export class ENothing extends Expression {
  *compile() {
    yield new IR.PushNothing();
  }
}

export class EVariable extends Expression {
  constructor(readonly name: Name) {
    super();
  }

  *compile() {
    yield new IR.PushLocal(this.name);
  }
}

export class EInterpolateText extends Expression {
  constructor(readonly source: string, readonly parts: InterpolatePart[]) {
    super();
  }

  compile_simple() {
    return new IR.SimpleInterpolation(
      this.parts.map((x) => x.compile_simple())
    );
  }

  *compile() {
    for (const part of this.parts) {
      yield* part.compile();
    }
    yield new IR.Interpolate(this.parts.length);
  }
}

export abstract class InterpolatePart {
  abstract compile(): Generator<IR.Operation>;
  abstract compile_simple(): IR.SimpleInterpolationPart;
}

export class InterpolateStatic extends InterpolatePart {
  constructor(readonly text: string) {
    super();
  }

  compile_simple() {
    return new IR.SimpleInterpolationStatic(this.text);
  }

  *compile() {
    yield new IR.PushText(this.text);
  }
}

export class InterpolateDynamic extends InterpolatePart {
  constructor(readonly expr: Expression) {
    super();
  }

  compile_simple() {
    const expr = this.expr;
    if (expr instanceof EVariable) {
      return new IR.SimpleInterpolationVariable(expr.name);
    } else {
      console.error(expr);
      throw new Error(`Non variable in simple interpolation`);
    }
  }

  *compile() {
    yield* this.expr.compile();
  }
}

export class EActor extends Expression {
  constructor(readonly name: string) {
    super();
  }

  *compile() {
    yield new IR.PushActor(this.name);
  }
}

export class ELet extends Expression {
  constructor(readonly name: Name, readonly value: Expression) {
    super();
  }

  *compile() {
    yield* this.value.compile();
    yield new IR.Let(this.name);
  }
}

export class ESearch extends Expression {
  constructor(readonly predicate: IR.Predicate) {
    super();
  }

  *compile() {
    yield new IR.Search(this.predicate);
  }
}

export class EIf extends Expression {
  constructor(
    readonly test: Expression,
    readonly consequent: Expression,
    readonly alternate: Expression
  ) {
    super();
  }

  *compile() {
    yield* this.test.compile();
    yield new IR.Block([], [...this.consequent.compile(), new IR.Return()]);
    yield new IR.Block([], [...this.alternate.compile(), new IR.Return()]);
    yield new IR.Branch();
  }
}

export class EProject extends Expression {
  constructor(readonly expr: Expression, readonly name: string) {
    super();
  }

  *compile() {
    yield* this.expr.compile();
    yield new IR.Project(this.name);
  }
}

export class EMatch extends Expression {
  constructor(readonly clauses: MatchClause[]) {
    super();
  }

  *compile() {
    for (const clause of this.clauses) {
      yield* clause.compile_body();
    }
    yield new IR.Match(this.clauses.map((x) => x.compile()));
  }
}

export abstract class MatchClause {
  abstract compile_body(): Generator<IR.Operation>;
  abstract compile(): IR.MatchClause;
}
export class MatchPredicate extends MatchClause {
  constructor(readonly predicate: IR.Predicate, readonly body: Statement[]) {
    super();
  }

  *compile_body() {
    yield new IR.Block(
      this.predicate.variables(),
      to_list(
        this.body.map((x) => x.compile()),
        [new IR.PushNothing(), new IR.Return()]
      )
    );
  }

  compile() {
    return new IR.MatchPredicate(this.predicate);
  }
}

export class MatchDefault extends MatchClause {
  constructor(readonly body: Statement[]) {
    super();
  }

  *compile_body() {
    yield new IR.Block(
      [],
      to_list(
        this.body.map((x) => x.compile()),
        [new IR.PushNothing(), new IR.Return()]
      )
    );
  }

  compile() {
    return new IR.MatchDefault();
  }
}

//== Utilities
function to_list(
  xss: Generator<IR.Operation>[],
  tail: IR.Operation[]
): IR.Operation[] {
  const result = [];
  for (const xs of xss) {
    for (const x of xs) {
      result.push(x);
    }
  }
  result.push.apply(result, tail);
  return result;
}

function sum(xs: number[]) {
  return xs.reduce((x, y) => x + y, 0);
}
