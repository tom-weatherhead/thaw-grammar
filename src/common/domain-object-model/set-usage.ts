// tom-weatherhead/thaw-grammar/src/common/domain-object-model/set-usage.ts

import { ifDefinedThenElse } from 'thaw-common-utilities.ts';

import { IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
import { IVariable } from './variable';

const typenameSetUsage = 'SetUsage';

export function isSetUsage<T>(obj: unknown): obj is SetUsage<T> {
	const setUsage = obj as SetUsage<T>;

	return typeof setUsage !== 'undefined' && setUsage.typename === typenameSetUsage;
}

export class SetUsage<T> implements IExpression<T> {
	public readonly typename: string = typenameSetUsage;

	constructor(
		public readonly variableName: IVariable<T>,
		public readonly expression: IExpression<T>
	) {}

	public toString(): string {
		return `(set ${this.variableName} ${this.expression})`;
	}

	public evaluate(
		globalInfo: IGlobalInfo<T>,
		localEnvironment?: IEnvironmentFrame<T>,
		options?: unknown
	): T {
		const expressionValue = this.expression.evaluate(globalInfo, localEnvironment, options);

		// If the variable is not already defined in a local env, we may have to assign it to the global env.

		// console.log(`SetUsage<T>.Evaluate() : var is ${this.variableName.Name}; value is ${expressionValue}`);
		ifDefinedThenElse(localEnvironment, globalInfo.globalEnvironment).addBubbleDown(
			this.variableName,
			expressionValue
		);

		return expressionValue;
	}
}
