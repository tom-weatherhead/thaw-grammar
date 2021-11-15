// clu/domain-object-model/while-usage.ts

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUGlobalInfo,
	ICluster,
	ICLUValue
} from './interfaces/ivalue';

export class CLUWhileUsage implements ICLUExpression {
	constructor(public readonly condition: ICLUExpression, public readonly body: ICLUExpression) {}

	/*
	public override string ToString()
	{
		return string.Format("(while {0} {1})", Condition, Body);
	}
	 */

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster | undefined,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		while (
			!globalInfo.valueIsFalse(this.condition.evaluate(localEnvironment, cluster, globalInfo))
		) {
			this.body.evaluate(localEnvironment, cluster, globalInfo);
		}

		return globalInfo.falseValue;
	}
}
