// clu/domain-object-model/function-definition-base.ts

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUFunctionDefinition,
	ICLUGlobalInfo,
	ICluster,
	ICLUValue
} from './interfaces/ivalue';

export abstract class CLUFunctionDefinitionBase implements ICLUExpression, ICLUFunctionDefinition {
	// public readonly string FunctionName;

	protected constructor(public readonly functionName: string) {
		// FunctionName = funcName;
	}

	public abstract evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster,
		globalInfo: ICLUGlobalInfo
	): ICLUValue;
}
