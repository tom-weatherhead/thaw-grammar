// clu/domain-object-model/begin-usage.ts

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUGlobalInfo,
	ICluster,
	ICLUValue
} from './interfaces/ivalue';

export class CLUBeginUsage implements ICLUExpression {
	constructor(
		public readonly firstExpression: ICLUExpression,
		public readonly expressionList: ICLUExpression[]
	) {}

	/*
	public override string ToString()
	{
		return string.Format("(begin {0} {1})", FirstExpression, ExpressionList);
	}
	 */

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster | undefined,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		let result = this.firstExpression.evaluate(localEnvironment, cluster, globalInfo);

		for (const expression of this.expressionList) {
			result = expression.evaluate(localEnvironment, cluster, globalInfo);
		}

		return result;
	}
}
