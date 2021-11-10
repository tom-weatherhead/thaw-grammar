// clu/domain-object-model/function-definition-base.ts

import { ICLUEnvironmentFrame, ICLUExpression, ICLUGlobalInfo, ICluster, ICLUValue } from './interfaces/ivalue';

export abstract class CLUFunctionDefinitionBase implements ICLUExpression {
	// public readonly string FunctionName;

	protected constructor(public readonly functionName: string) {
		// FunctionName = funcName;
	}

	public abstract evaluate(localEnvironment: ICLUEnvironmentFrame, cluster: ICluster, globalInfo: ICLUGlobalInfo): ICLUValue;
}
