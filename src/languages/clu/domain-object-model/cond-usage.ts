// clu/domain-object-model/cond-usage.ts

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUGlobalInfo,
	ICluster,
	ICLUValue
} from './interfaces/ivalue';

export class CLUCondUsage implements ICLUExpression {
	// public readonly List<KeyValuePair<ICLUExpression, ICLUExpression>> ExprPairList;

	constructor(public readonly exprPairList: [ICLUExpression, ICLUExpression][]) {
		// ExprPairList = exprPairList;
	}

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		//var falseValue = globalInfo.FalseValue;

		for (const [key, value] of this.exprPairList) {
			//if (!exprPair.Key.Evaluate(localEnvironment, cluster, globalInfo).Equals(falseValue))
			if (!globalInfo.valueIsFalse(key.evaluate(localEnvironment, cluster, globalInfo))) {
				return value.evaluate(localEnvironment, cluster, globalInfo);
			}
		}

		return globalInfo.falseValue;
	}
}
