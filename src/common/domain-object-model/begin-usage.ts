// tom-weatherhead/thaw-grammar/src/common/domain-object-model/begin-usage.ts

import { IEnvironmentFrame } from './environment-frame';
// import { ExpressionList } from './expression-list';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

export class BeginUsage<T> implements IExpression<T> {
	// public readonly firstExpression: IExpression<T>;
	// public readonly expressionList: IExpression<T>[];

	constructor(
		public readonly firstExpression: IExpression<T>,
		public readonly expressionList: IExpression<T>[]
	) {
		// this.firstExpression = firstExpression;
		// this.expressionList = expressionList;
	}

	public toString(): string {
		return `(begin ${this.firstExpression} ${this.expressionList})`;
	}

	// public evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T {
	public evaluate(
		globalInfo: IGlobalInfo<T>,
		localEnvironment?: IEnvironmentFrame<T>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): T {
		// const env = ifDefinedThenElse(localEnvironment, globalInfo.globalEnvironment);

		return this.expressionList.reduce(
			// eslint-disable-next-line @typescript-eslint/no-unused-vars
			(previousResult: T, expression: IExpression<T>) =>
				expression.evaluate(globalInfo, localEnvironment),
			this.firstExpression.evaluate(globalInfo, localEnvironment)
		);
	}
}
