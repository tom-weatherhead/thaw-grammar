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
	protected constructor(public readonly functionName: string) {}

	public abstract evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster | undefined,
		globalInfo: ICLUGlobalInfo
	): ICLUValue;
}
