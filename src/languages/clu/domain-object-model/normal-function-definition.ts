// clu/domain-object-model/normal-function-definition.ts

// TODO: Use the common FunctionDefinition<T> instead.

import { Name } from 'thaw-interpreter-core';

import { IFunctionDefinition } from '../../../common/domain-object-model/function-definition';

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUGlobalInfo,
	// ICluster,
	ICLUValue,
	ICLUVariable
} from './interfaces/ivalue';

import { CLUFunctionDefinitionBase } from './function-definition-base';

const typenameCLUNormalFunctionDefinition = 'CLUNormalFunctionDefinition';

export function isCLUNormalFunctionDefinition(obj: unknown): obj is CLUNormalFunctionDefinition {
	const v = obj as CLUNormalFunctionDefinition;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameCLUNormalFunctionDefinition
	);
}

export class CLUNormalFunctionDefinition
	extends CLUFunctionDefinitionBase
	implements IFunctionDefinition<ICLUValue>
{
	public readonly typename: string = typenameCLUNormalFunctionDefinition;

	constructor(
		functionName: Name,
		public readonly argList: ICLUVariable[],
		public readonly body: ICLUExpression
	) {
		super(functionName);
	}

	/*
	public override string ToString()
	{
		return string.Format("(define {0} {1} {2})", FunctionName, ArgList, Body);
	}
	 */

	// public evaluate(
	// 	localEnvironment: ICLUEnvironmentFrame,
	// 	cluster: ICluster | undefined,
	// 	globalInfo: ICLUGlobalInfo
	// ): ICLUValue {
	public evaluate(
		globalInfo: ICLUGlobalInfo,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		localEnvironment?: ICLUEnvironmentFrame,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): ICLUValue {
		globalInfo.functionDefinitions.set(this.functionName.value, this);

		return globalInfo.trueValue;
	}
}
