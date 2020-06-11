// tom-weatherhead/thaw-grammar/src/common/domain-object-model/begin-usage.ts

'use strict';

import { EnvironmentFrame } from './environment-frame';
import { ExpressionList } from './expression-list';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

export class BeginUsage<T> implements IExpression<T> {
	public readonly firstExpression: IExpression<T>;
	public readonly expressionList: ExpressionList<T>;

	constructor(
		firstExpression: IExpression<T>,
		expressionList: ExpressionList<T>
	) {
		this.firstExpression = firstExpression;
		this.expressionList = expressionList;
	}

	public toString(): string {
		return `(begin ${this.firstExpression} ${this.expressionList})`;
	}

	public evaluate(
		localEnvironment: EnvironmentFrame<T>,
		globalInfo: IGlobalInfo<T>
	): T {
		return this.expressionList.value.reduce(
			(previousResult: T, expression: IExpression<T>) =>
				expression.evaluate(localEnvironment, globalInfo), // Lint: Yes, previousResult is unused.
			this.firstExpression.evaluate(localEnvironment, globalInfo)
		);
	}
}
