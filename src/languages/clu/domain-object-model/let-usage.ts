// clu/domain-object-model/let-usage.ts

import { ICLUEnvironmentFrame, ICLUExpression, ICLUGlobalInfo, ICluster, ICLUValue } from './interfaces/ivalue';

export class CLULetUsage implements ICLUExpression {
	public readonly List<KeyValuePair<CLUVariable, ICLUExpression>> Bindings;
	public readonly ICLUExpression Expression;

	public CLULetUsage(List<KeyValuePair<CLUVariable, ICLUExpression>> bindings, ICLUExpression expression)
	{
		Bindings = bindings;
		Expression = expression;
	}

	public evaluate(localEnvironment: ICLUEnvironmentFrame, cluster: ICluster, globalInfo: ICLUGlobalInfo): ICLUValue {
		var newEnvFrame = new CLUEnvironmentFrame(localEnvironment);

		foreach (var binding in Bindings)
		{
			newEnvFrame.Add(binding.Key, binding.Value.Evaluate(localEnvironment, cluster, globalInfo));
		}

		return Expression.Evaluate(newEnvFrame, cluster, globalInfo);
	}
}
