// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/lambda-expression.ts

import { ifDefinedThenElse } from 'thaw-common-utilities.ts';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { IVariable } from '../../../common/domain-object-model/variable';
import { ISExpression } from '../../lisp/domain-object-model/isexpression';

import { Closure } from './closure';

const typenameLambdaExpression = 'LambdaExpression';

export function isLambdaExpression(obj: unknown): obj is LambdaExpression {
	const le = obj as LambdaExpression;

	return typeof le !== 'undefined' && le.typename === typenameLambdaExpression;
}

export class LambdaExpression implements IExpression<ISExpression> {
	public readonly typename: string = typenameLambdaExpression;

	constructor(
		public readonly argList: IVariable<ISExpression>[],
		public readonly body: IExpression<ISExpression>,
		public readonly line = 0,
		public readonly column = 0
	) {}

	public toString(): string {
		return `(lambda ${this.argList} ${this.body})`;
	}

	public evaluate(
		globalInfo: IGlobalInfo<ISExpression>,
		localEnvironment?: IEnvironmentFrame<ISExpression>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): ISExpression {
		return new Closure(
			this.argList,
			this.body,
			ifDefinedThenElse(localEnvironment, globalInfo.globalEnvironment),
			this.line,
			this.column
		);
	}
}
