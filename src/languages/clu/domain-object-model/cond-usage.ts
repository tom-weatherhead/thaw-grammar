// clu/domain-object-model/cond-usage.ts

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUGlobalInfo,
	ICluster,
	ICLUValue
} from './interfaces/ivalue';

export class CLUCondUsage implements ICLUExpression {
	constructor(public readonly exprPairList: [ICLUExpression, ICLUExpression][]) {}

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster | undefined,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		for (const [key, value] of this.exprPairList) {
			if (!globalInfo.valueIsFalse(key.evaluate(localEnvironment, cluster, globalInfo))) {
				return value.evaluate(localEnvironment, cluster, globalInfo);
			}
		}

		return globalInfo.falseValue;
	}
}
