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

	constructor(
		public readonly body: IExpression<ISExpression>,
		public /* readonly */ thunkEnvironment: EnvironmentFrame<ISExpression> // Not readonly, due to letrec.
	) {
		super();
	}

	// Do we need to override Equals() and GetHashCode() ?

	public toString(): string {
		return '<thunk>';
	}

	// public equals(obj: unknown): boolean {
	//     return object.ReferenceEquals(this, obj);
	// }

	public dethunk(globalInfo: IGlobalInfo<ISExpression>): ISExpression {
		// eslint-disable-next-line @typescript-eslint/no-this-alias
		let sexpr: ISExpression = this;

		while (isThunk(sexpr)) {
			const thunk = sexpr as Thunk;

			sexpr = thunk.body.evaluate(this.thunkEnvironment, globalInfo);
		}

		return sexpr;
	}

	public override evaluate(
		localEnvironment: EnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		// const result = this.body.evaluate(this.thunkEnvironment, globalInfo);
		//
		// return SASLGlobalInfo.deThunk(result, globalInfo);

		return this.dethunk(globalInfo);
	}
}
