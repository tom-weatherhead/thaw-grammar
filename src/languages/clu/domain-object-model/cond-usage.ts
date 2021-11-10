// clu/domain-object-model/cond-usage.ts

import { ICLUEnvironmentFrame, ICLUExpression, ICLUGlobalInfo, ICluster, ICLUValue } from './interfaces/ivalue';

export class CLUCondUsage implements ICLUExpression {
	public readonly List<KeyValuePair<ICLUExpression, ICLUExpression>> ExprPairList;

	public CLUCondUsage(List<KeyValuePair<ICLUExpression, ICLUExpression>> exprPairList)
	{
		ExprPairList = exprPairList;
	}

	public evaluate(localEnvironment: ICLUEnvironmentFrame, cluster: ICluster, globalInfo: ICLUGlobalInfo): ICLUValue {
		//var falseValue = globalInfo.FalseValue;

		foreach (var exprPair in ExprPairList)
		{

			//if (!exprPair.Key.Evaluate(localEnvironment, cluster, globalInfo).Equals(falseValue))
			if (!globalInfo.ValueIsFalse(exprPair.Key.Evaluate(localEnvironment, cluster, globalInfo)))
			{
				return exprPair.Value.Evaluate(localEnvironment, cluster, globalInfo);
			}
		}

		//return falseValue;
		return globalInfo.FalseValue;
	}
}
