// tom-weatherhead/thaw-grammar/src/common/domain-object-model/set-usage.ts

'use strict';

import { EnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
import { Variable } from './variable';

export class SetUsage<T> implements IExpression<T> {
	public readonly variableName: Variable<T>;
	public readonly expression: IExpression<T>;

	constructor(variableName: Variable<T>, expression: IExpression<T>) {
		this.variableName = variableName;
		this.expression = expression;
	}

	public toString(): string {
		return `(set ${this.variableName} ${this.expression})`;
	}

	public evaluate(
		localEnvironment: EnvironmentFrame<T>,
		globalInfo: IGlobalInfo<T>
	): T {
		const expressionValue = this.expression.evaluate(
			localEnvironment,
			globalInfo
		);

		// If the variable is not already defined in a local env, we may have to assign it to the global env.
		// console.log(`SetUsage<T>.Evaluate() : var is ${this.variableName.Name}; value is ${expressionValue}`);
		localEnvironment.addBubbleDown(this.variableName, expressionValue);

		return expressionValue;
	}
}
