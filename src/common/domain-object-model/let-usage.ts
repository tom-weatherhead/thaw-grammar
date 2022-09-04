// tom-weatherhead/thaw-grammar/src/common/domain-object-model/let-usage.ts

import { EnvironmentFrame, IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
import { IVariable } from './variable';

const typenameLetUsage = 'LetUsage';

export function isLetUsage<T>(obj: unknown): obj is LetUsage<T> {
	const letUsage = obj as LetUsage<T>;

	return typeof letUsage !== 'undefined' && letUsage.typename === typenameLetUsage;
}

export class LetUsage<T> implements IExpression<T> {
	public readonly typename: string = typenameLetUsage;

	constructor(
		public readonly bindings: [IVariable<T>, IExpression<T>][],
		public readonly expression: IExpression<T>
	) {}

	public toString(): string {
		const fnBindingAsString = ([v, expr]: [IVariable<T>, IExpression<T>]) => `(${v} ${expr})`;
		const bindingsAsString = this.bindings.map(fnBindingAsString).join(' ');

		return `(let (${bindingsAsString}) ${this.expression})`;
	}

	public evaluate(
		globalInfo: IGlobalInfo<T>,
		localEnvironment?: IEnvironmentFrame<T>,
		options?: unknown
	): T {
		const newEnvFrame = new EnvironmentFrame<T>(localEnvironment);

		// this.bindings.for Each(([v, expr]: [Variable<T>, IExpression<T>]) => {
		for (const [v, expr] of this.bindings) {
			newEnvFrame.add(v, expr.evaluate(globalInfo, localEnvironment, options));
		} // );

		return this.expression.evaluate(globalInfo, newEnvFrame, options);
	}
}
