// clu/domain-object-model/if-usage.ts

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUGlobalInfo,
	ICluster,
	ICLUValue
} from './interfaces/ivalue';

export class CLUIfUsage implements ICLUExpression {
	// public readonly ICLUExpression Condition;
	// public readonly ICLUExpression IfBody;
	// public readonly ICLUExpression ElseBody;

	constructor(
		public readonly condition: ICLUExpression,
		public readonly ifBody: ICLUExpression,
		public readonly elseBody: ICLUExpression
	) {
		// Condition = condition;
		// IfBody = ifBody;
		// ElseBody = elseBody;
	}

	/*
	public override string ToString()
	{
		return string.Format("(if {0} {1} {2})", Condition, IfBody, ElseBody);
	}
	 */

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		const conditionValue = this.condition.evaluate(localEnvironment, cluster, globalInfo);

		//if (!conditionValue.Equals(globalInfo.FalseValue))
		if (!globalInfo.valueIsFalse(conditionValue)) {
			return this.ifBody.evaluate(localEnvironment, cluster, globalInfo);
		} else {
			return this.elseBody.evaluate(localEnvironment, cluster, globalInfo);
		}
	}
}
