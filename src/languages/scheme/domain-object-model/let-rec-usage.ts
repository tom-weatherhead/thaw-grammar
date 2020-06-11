// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/let-rec-usage.ts

'use strict';

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { Variable } from '../../../common/domain-object-model/variable';

export class LetRecUsage<T> implements IExpression<T> {
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

		return `(letrec (${bindingsAsString}) ${this.expression})`;
	}

	// public ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
	// {
	//     var falseValue = globalInfo.FalseValue;
	//     var newEnvFrame = new EnvironmentFrame<ISExpression>(localEnvironment);

	//     foreach (var binding in Bindings)   // Add all variables that are bound in Bindings to newEnvFrame before any closures are created in the next loop.
	//     {
	//         newEnvFrame.Add(binding.Key, falseValue);
	//     }

	//     foreach (var binding in Bindings)
	//     {
	//         newEnvFrame.Add(binding.Key, binding.Value.Evaluate(newEnvFrame, globalInfo));
	//     }

	//     return Expression.Evaluate(newEnvFrame, globalInfo);
	// }

	public evaluate(
		localEnvironment: EnvironmentFrame<T>,
		globalInfo: IGlobalInfo<T>
	): T {
		const newEnvFrame = new EnvironmentFrame<T>(localEnvironment);

		this.bindings.forEach(([v, expr]: [Variable<T>, IExpression<T>]) => {
			// Add all variables that are bound in this.bindings to newEnvFrame before any closures are created in the next loop.
			newEnvFrame.add(v, globalInfo.falseValue);
		});

		this.bindings.forEach(([v, expr]: [Variable<T>, IExpression<T>]) => {
			newEnvFrame.add(v, expr.evaluate(newEnvFrame, globalInfo));
		});

		return this.expression.evaluate(newEnvFrame, globalInfo);
	}
}
