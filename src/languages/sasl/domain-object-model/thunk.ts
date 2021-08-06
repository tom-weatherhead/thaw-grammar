// thunk.ts

// A thunk (or suspension).

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
// import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
// import { Name } from '../../../common/domain-object-model/name';

// import { EvaluationException } from '../../../common/exceptions/evaluation-exception';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';
// import { LISPOperatorUsage } from '../../lisp/domain-object-model/lisp-operator-usage';
import { SExpressionBase } from '../../lisp/domain-object-model/sexpression-base';

// import { SASLGlobalInfo } from './global-info';

const typenameThunk = 'Thunk';

export function isThunk(obj: unknown): obj is Thunk {
	const otherThunk = obj as Thunk;

	return (
		typeof otherThunk !== 'undefined' &&
		typeof otherThunk.typename !== 'undefined' &&
		otherThunk.typename === typenameThunk
	);
}

export class Thunk extends SExpressionBase {
	public readonly typename = typenameThunk;
	private dethunkedValue: ISExpression | undefined;

	constructor(
		public readonly body: IExpression<ISExpression>,
		public /* readonly */ thunkEnvironment: EnvironmentFrame<ISExpression> // Not readonly, due to letrec.
	) {
		super();
	}

	// Do we need to override Equals() and GetHashCode() ?

	public toString(): string {
		if (typeof this.dethunkedValue !== 'undefined') {
			return this.dethunkedValue.toString();
		}

		return '<thunk>';
	}

	// public equals(obj: unknown): boolean {
	//     return object.ReferenceEquals(this, obj);
	// }

	public override isNumber(): boolean {
		return typeof this.dethunkedValue !== 'undefined' && this.dethunkedValue.isNumber();
	}

	public override isSymbol(): boolean {
		return typeof this.dethunkedValue !== 'undefined' && this.dethunkedValue.isSymbol();
	}

	public override isList(): boolean {
		return typeof this.dethunkedValue !== 'undefined' && this.dethunkedValue.isList();
	}

	public override isNull(): boolean {
		return typeof this.dethunkedValue !== 'undefined' && this.dethunkedValue.isNull();
	}

	public override isPrimOp(): boolean {
		return typeof this.dethunkedValue !== 'undefined' && this.dethunkedValue.isPrimOp();
	}

	public override isClosure(): boolean {
		return typeof this.dethunkedValue !== 'undefined' && this.dethunkedValue.isClosure();
	}

	public override isString(): boolean {
		return typeof this.dethunkedValue !== 'undefined' && this.dethunkedValue.isString();
	}

	public dethunk(globalInfo: IGlobalInfo<ISExpression>): ISExpression {
		if (typeof this.dethunkedValue !== 'undefined') {
			return this.dethunkedValue;
		}

		// eslint-disable-next-line @typescript-eslint/no-this-alias
		let sexpr: ISExpression = this;

		while (isThunk(sexpr)) {
			const thunk = sexpr as Thunk;

			sexpr = thunk.body.evaluate(this.thunkEnvironment, globalInfo);
		}

		this.dethunkedValue = sexpr;

		return sexpr;
	}

	public override evaluate(
		localEnvironment: EnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		// const result = this.body.evaluate(this.thunkEnvironment, globalInfo);
		//
		// return SASLGlobalInfo.deThunk(result, globalInfo);

		if (typeof this.dethunkedValue !== 'undefined') {
			return this.dethunkedValue;
		}

		return this.dethunk(globalInfo);
	}
}
