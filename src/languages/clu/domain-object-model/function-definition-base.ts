// clu/domain-object-model/function-definition-base.ts

import { Name } from 'thaw-interpreter-core';

// import { IExpression } from '../../../common/domain-object-model/iexpression';
//
// import { IVariable } from '../../../common/domain-object-model/variable';

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	// ICLUFunctionDefinition,
	ICLUGlobalInfo,
	// ICluster,
	ICLUValue
} from './interfaces/ivalue';

export abstract class CLUFunctionDefinitionBase implements ICLUExpression {
	protected constructor(
		public readonly functionName: Name /*,
	public readonly argList: IVariable<ICLUValue>[],
	public readonly body: IExpression<ICLUValue> */
	) {}

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
