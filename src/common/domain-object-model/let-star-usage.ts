// tom-weatherhead/thaw-grammar/src/common/domain-object-model/let-star-usage.ts

import { EnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
import { Variable } from './variable';

export class LetStarUsage<T> implements IExpression<T> {
	public readonly bindings: [Variable<T>, IExpression<T>][];
	public readonly expression: IExpression<T>;

	constructor(bindings: [Variable<T>, IExpression<T>][], expression: IExpression<T>) {
		this.bindings = bindings;
		this.expression = expression;
	}

	public toString(): string {
		const fnBindingAsString = ([v, expr]: [Variable<T>, IExpression<T>]) => `(${v} ${expr})`;
		const bindingsAsString = this.bindings.map(fnBindingAsString).join(' ');

		return `(let* (${bindingsAsString}) ${this.expression})`;
	}

	public evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T {
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
			(previousEnvFrame: EnvironmentFrame<T>, [v, expr]: [Variable<T>, IExpression<T>]) => {
				const newEnvFrame = new EnvironmentFrame<T>(previousEnvFrame);

				newEnvFrame.add(v, expr.evaluate(previousEnvFrame, globalInfo));

				return newEnvFrame;
			},
			localEnvironment
		);

		return this.expression.evaluate(lastNewEnvFrame, globalInfo);
	}
}
