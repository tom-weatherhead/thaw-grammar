// tom-weatherhead/thaw-grammar/src/common/domain-object-model/begin-usage.ts

import { IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

const typenameBeginUsage = 'BeginUsage';

export function isBeginUsage<T>(obj: unknown): obj is BeginUsage<T> {
	const beginUsage = obj as BeginUsage<T>;

	return typeof beginUsage !== 'undefined' && beginUsage.typename === typenameBeginUsage;
}

export class BeginUsage<T> implements IExpression<T> {
	public readonly typename: string = typenameBeginUsage;

	constructor(
		public readonly firstExpression: IExpression<T>,
		public readonly expressionList: IExpression<T>[]
	) {}

	public toString(): string {
		return `(begin ${this.firstExpression} ${this.expressionList})`;
	}

	public evaluate(
		globalInfo: IGlobalInfo<T>,
		localEnvironment?: IEnvironmentFrame<T>,
		options?: unknown
	): T {
		return this.expressionList.reduce(
			// eslint-disable-next-line @typescript-eslint/no-unused-vars
			(previousResult: T, expression: IExpression<T>) =>
				expression.evaluate(globalInfo, localEnvironment, options),
			this.firstExpression.evaluate(globalInfo, localEnvironment, options)
		);
	}
}
