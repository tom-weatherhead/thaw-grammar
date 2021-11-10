// clu/domain-object-model/set-usage.ts

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUGlobalInfo,
	ICluster,
	ICLUValue,
	ICLUVariable
} from './interfaces/ivalue';

export class CLUSetUsage implements ICLUExpression {
	// public readonly CLUVariable VariableName;
	// public readonly ICLUExpression Expression;

	constructor(
		public readonly variableName: ICLUVariable,
		public readonly expression: ICLUExpression
	) {
		// VariableName = variableName;
		// Expression = expression;
	}

	/*
	public override string ToString()
	{
		return string.Format("(set {0} {1})", VariableName, Expression);
	}
	 */

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		const expressionValue = this.expression.evaluate(localEnvironment, cluster, globalInfo);

		// If the variable is not already defined in the local env, we may have to assign it to the global env (assuming that there are only two envs).
		localEnvironment.addBubbleDown(this.variableName, expressionValue); // TODO: Warning: This may be too simple and very wrong.
		return expressionValue;
	}
}
