// clu/domain-object-model/function-definition-base.ts

import { Name } from 'thaw-interpreter-core';

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUFunctionDefinition,
	ICLUGlobalInfo,
	// ICluster,
	ICLUValue
} from './interfaces/ivalue';

export abstract class CLUFunctionDefinitionBase implements ICLUExpression, ICLUFunctionDefinition {
	protected constructor(public readonly functionName: Name) {}

	// public abstract evaluate(
	// 	localEnvironment: ICLUEnvironmentFrame,
	// 	cluster: ICluster | undefined,
	// 	globalInfo: ICLUGlobalInfo
	// ): ICLUValue;
	public abstract evaluate(
		globalInfo: ICLUGlobalInfo,
		localEnvironment?: ICLUEnvironmentFrame,
		options?: unknown
	): ICLUValue;
}
