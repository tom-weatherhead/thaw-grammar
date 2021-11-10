// clu/domain-object-model/let-usage.ts

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUGlobalInfo,
	ICluster,
	ICLUValue,
	ICLUVariable
} from './interfaces/ivalue';

import { CLUEnvironmentFrame } from './environment-frame';

export class CLULetUsage implements ICLUExpression {
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
		cluster: ICluster,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		const newEnvFrame = new CLUEnvironmentFrame(localEnvironment);

		for (const [key, value] of this.bindings) {
			newEnvFrame.add(key, value.evaluate(localEnvironment, cluster, globalInfo));
		}

		return this.expression.evaluate(newEnvFrame, cluster, globalInfo);
	}
}
