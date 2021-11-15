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
	constructor(
		public readonly bindings: [ICLUVariable, ICLUExpression][],
		public readonly expression: ICLUExpression
	) {}

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster | undefined,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		const newEnvFrame = new CLUEnvironmentFrame(localEnvironment);

		for (const [key, value] of this.bindings) {
			newEnvFrame.add(key, value.evaluate(localEnvironment, cluster, globalInfo));
		}

		return this.expression.evaluate(newEnvFrame, cluster, globalInfo);
	}
}
