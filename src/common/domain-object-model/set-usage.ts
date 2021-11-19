// tom-weatherhead/thaw-grammar/src/common/domain-object-model/set-usage.ts

import { ifDefinedThenElse } from 'thaw-common-utilities.ts';

import { IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
import { IVariable } from './variable';

export class SetUsage<T> implements IExpression<T> {
	// public readonly variableName: Variable<T>;
	// public readonly expression: IExpression<T>;

	constructor(
		public readonly variableName: IVariable<T>,
		public readonly expression: IExpression<T>
	) {
		// this.variableName = variableName;
		// this.expression = expression;
	}

	public toString(): string {
		return `(set ${this.variableName} ${this.expression})`;
	}

	// public evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T {
	public evaluate(
		globalInfo: IGlobalInfo<T>,
		localEnvironment?: IEnvironmentFrame<T>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): T {
		const expressionValue = this.expression.evaluate(globalInfo, localEnvironment);

		// If the variable is not already defined in a local env, we may have to assign it to the global env.
		// console.log(`SetUsage<T>.Evaluate() : var is ${this.variableName.Name}; value is ${expressionValue}`);
		ifDefinedThenElse(localEnvironment, globalInfo.globalEnvironment).addBubbleDown(
			this.variableName,
			expressionValue
		);

		return expressionValue;
	}
}
