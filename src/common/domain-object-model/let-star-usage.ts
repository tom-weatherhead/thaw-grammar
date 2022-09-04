// tom-weatherhead/thaw-grammar/src/common/domain-object-model/let-star-usage.ts

import { ifDefinedThenElse } from 'thaw-common-utilities.ts';

import { EnvironmentFrame, IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
import { IVariable } from './variable';

const typenameLetStarUsage = 'LetStarUsage';

export function isLetStarUsage<T>(obj: unknown): obj is LetStarUsage<T> {
	const letStarUsage = obj as LetStarUsage<T>;

	return typeof letStarUsage !== 'undefined' && letStarUsage.typename === typenameLetStarUsage;
}

export class LetStarUsage<T> implements IExpression<T> {
	public readonly typename: string = typenameLetStarUsage;

	constructor(
		public readonly bindings: [IVariable<T>, IExpression<T>][],
		public readonly expression: IExpression<T>
	) {}

	public toString(): string {
		const fnBindingAsString = ([v, expr]: [IVariable<T>, IExpression<T>]) => `(${v} ${expr})`;
		const bindingsAsString = this.bindings.map(fnBindingAsString).join(' ');

		return `(let* (${bindingsAsString}) ${this.expression})`;
	}

	public evaluate(
		globalInfo: IGlobalInfo<T>,
		localEnvironment?: IEnvironmentFrame<T>,
		options?: unknown
	): T {
		// 1) No:
		// const newEnvFrame = new EnvironmentFrame<T>(localEnvironment);

		// this.bindings.for Each(([v, expr]: [Variable<T>, IExpression<T>]) => {
		// 	// For this line, LetUsage.evaluate() does this instead:
		// 	// newEnvFrame.add(v, expr.evaluate(localEnvironment, globalInfo));

		// 	newEnvFrame.add(v, expr.evaluate(newEnvFrame, globalInfo));
		// });

		// return this.expression.evaluate(newEnvFrame, globalInfo);

		// 2) Correct C#:
		// var lastEnv = localEnvironment;

		// for each (var binding in Bindings)
		// {
		//     var newEnvFrame = new EnvironmentFrame<T>(lastEnv);

		//     newEnvFrame.Add(binding.Key, binding.Value.Evaluate(lastEnv, globalInfo));
		//     lastEnv = newEnvFrame;
		// }

		// return Expression.Evaluate(lastEnv, globalInfo);

		// 3)
		const lastNewEnvFrame = this.bindings.reduce(
			(previousEnvFrame: IEnvironmentFrame<T>, [v, expr]: [IVariable<T>, IExpression<T>]) => {
				const newEnvFrame = new EnvironmentFrame<T>(previousEnvFrame);

				newEnvFrame.add(v, expr.evaluate(globalInfo, previousEnvFrame, options));

				return newEnvFrame;
			},
			ifDefinedThenElse(localEnvironment, globalInfo.globalEnvironment)
		);

		return this.expression.evaluate(globalInfo, lastNewEnvFrame, options);
	}
}
