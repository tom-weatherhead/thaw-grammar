// clu/domain-object-model/let-star-usage.ts

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUGlobalInfo,
	ICluster,
	ICLUValue,
	ICLUVariable
} from './interfaces/ivalue';

import { CLUEnvironmentFrame } from './environment-frame';

export class CLULetStarUsage implements ICLUExpression {
	// public readonly List<KeyValuePair<CLUVariable, ICLUExpression>> Bindings;
	// public readonly ICLUExpression Expression;

	constructor(
		public readonly bindings: [ICLUVariable, ICLUExpression][],
		public readonly expression: ICLUExpression
	) {
		// Bindings = bindings;
		// Expression = expression;
	}

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster | undefined,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		// #if DEAD_CODE
		// var newEnvFrame = new CLUEnvironmentFrame(localEnvironment);
		//
		// foreach (var binding in Bindings)
		// {
		// 	newEnvFrame.Add(binding.Key, binding.Value.Evaluate(newEnvFrame, cluster, globalInfo));
		// }
		//
		// return Expression.Evaluate(newEnvFrame, cluster, globalInfo);
		// #else

		// 2014/02/17 : This implementation does not support recursive definitions.
		let lastEnv = localEnvironment;

		for (const [key, value] of this.bindings) {
			const newEnvFrame = new CLUEnvironmentFrame(lastEnv);

			newEnvFrame.add(key, value.evaluate(lastEnv, cluster, globalInfo));
			lastEnv = newEnvFrame;
		}

		return this.expression.evaluate(lastEnv, cluster, globalInfo);
		// #endif
	}
}
