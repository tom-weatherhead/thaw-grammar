// clu/domain-object-model/normal-function-definition.ts

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUGlobalInfo,
	ICluster,
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

export class CLUNormalFunctionDefinition extends CLUFunctionDefinitionBase {
	public readonly typename: string = typenameCLUNormalFunctionDefinition;
	// public readonly List<CLUVariable> ArgList;
	// public readonly ICLUExpression Body;

	constructor(
		functionName: string,
		public readonly argList: ICLUVariable[],
		public readonly body: ICLUExpression
	) {
		super(functionName);

		// ArgList = argList;
		// Body = body;
	}

	/*
	public override string ToString()
	{
		return string.Format("(define {0} {1} {2})", FunctionName, ArgList, Body);
	}
	 */

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		globalInfo.functionDefinitions[this.functionName] = this;

		return globalInfo.trueValue;
	}
}
