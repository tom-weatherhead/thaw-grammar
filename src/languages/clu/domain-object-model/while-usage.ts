// clu/domain-object-model/while-usage.ts

import { ICLUEnvironmentFrame, ICLUExpression, ICLUGlobalInfo, ICluster, ICLUValue } from './interfaces/ivalue';

export class CLUWhileUsage implements ICLUExpression {
	// public readonly ICLUExpression Condition;
	// public readonly ICLUExpression Body;

	constructor(public readonly condition: ICLUExpression, public readonly body: ICLUExpression) {
		// Condition = condition;
		// Body = body;
	}

	/*
	public override string ToString()
	{
		return string.Format("(while {0} {1})", Condition, Body);
	}
	 */

	 public evaluate(localEnvironment: ICLUEnvironmentFrame, cluster: ICluster, globalInfo: ICLUGlobalInfo): ICLUValue {
		// #if DEAD_CODE
		// ICLUValue conditionValue;
		// //ICLUValue falseValue = globalInfo.FalseValue;
		//
		// for (; ; )
		// {
		// 	conditionValue = Condition.Evaluate(localEnvironment, cluster, globalInfo);
		//
		// 	//if (conditionValue.Equals(falseValue))
		// 	if (globalInfo.ValueIsFalse(conditionValue))
		// 	{
		// 		break;
		// 	}
		//
		// 	Body.Evaluate(localEnvironment, cluster, globalInfo);
		// }
		//
		// return conditionValue;
		// #else

		while (!globalInfo.valueIsFalse(this.condition.evaluate(localEnvironment, cluster, globalInfo))) {
			this.body.evaluate(localEnvironment, cluster, globalInfo);
		}

		return globalInfo.falseValue;
		// #endif
	}
}
