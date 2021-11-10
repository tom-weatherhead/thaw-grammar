// clu/domain-object-model/set-usage.ts

import { ICLUEnvironmentFrame, ICLUExpression, ICLUGlobalInfo, ICluster, ICLUValue } from './interfaces/ivalue';

export class CLUSetUsage implements ICLUExpression {
	public readonly CLUVariable VariableName;
	public readonly ICLUExpression Expression;

	public CLUSetUsage(CLUVariable variableName, ICLUExpression expression)
	{
		VariableName = variableName;
		Expression = expression;
	}

	/*
	public override string ToString()
	{
		return string.Format("(set {0} {1})", VariableName, Expression);
	}
	 */

	 public evaluate(localEnvironment: ICLUEnvironmentFrame, cluster: ICluster, globalInfo: ICLUGlobalInfo): ICLUValue {
		ICLUValue expressionValue = Expression.Evaluate(localEnvironment, cluster, globalInfo);

		// If the variable is not already defined in the local env, we may have to assign it to the global env (assuming that there are only two envs).
		localEnvironment.AddBubbleDown(VariableName, expressionValue);    // TODO: Warning: This may be too simple and very wrong.
		return expressionValue;
	}
}
