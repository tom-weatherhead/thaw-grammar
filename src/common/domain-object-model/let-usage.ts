// tom-weatherhead/thaw-grammar/src/common/domain-object-model/let-usage.ts

'use strict';

import { EnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
import { Variable } from './variable';

export class LetUsage<T> implements IExpression<T> {
	public readonly bindings: [Variable<T>, IExpression<T>][];
	public readonly expression: IExpression<T>;

	constructor(
		bindings: [Variable<T>, IExpression<T>][],
		expression: IExpression<T>
	) {
		this.bindings = bindings;
		this.expression = expression;
	}

	public toString(): string {
		const fnBindingAsString = ([v, expr]: [
			Variable<T>,
			IExpression<T>
		]) => `(${v} ${expr})`;
		const bindingsAsString = this.bindings
			.map(fnBindingAsString)
			.join(' ');

		return `(let (${bindingsAsString}) ${this.expression})`;
	}

	public evaluate(
		localEnvironment: EnvironmentFrame<T>,
		globalInfo: IGlobalInfo<T>
	): T {
		const newEnvFrame = new EnvironmentFrame<T>(localEnvironment);

		this.bindings.forEach(([v, expr]: [Variable<T>, IExpression<T>]) => {
			newEnvFrame.add(v, expr.evaluate(localEnvironment, globalInfo));
		});

		return this.expression.evaluate(newEnvFrame, globalInfo);
	}
}
