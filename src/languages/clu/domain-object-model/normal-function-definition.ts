// clu/domain-object-model/normal-function-definition.ts

import { ICLUEnvironmentFrame, ICLUExpression, ICLUGlobalInfo, ICluster, ICLUValue } from './interfaces/ivalue';

export class CLUNormalFunctionDefinition extends CLUFunctionDefinitionBase {
	public readonly List<CLUVariable> ArgList;
	public readonly ICLUExpression Body;

	constructor(string functionName, List<CLUVariable> argList, ICLUExpression body) {
		super(functionName);

		ArgList = argList;
		Body = body;
	}

	/*
	public override string ToString()
	{
		return string.Format("(define {0} {1} {2})", FunctionName, ArgList, Body);
	}
	 */

	 public evaluate(localEnvironment: ICLUEnvironmentFrame, cluster: ICluster, globalInfo: ICLUGlobalInfo): ICLUValue {
		globalInfo.FunctionDefinitions[FunctionName] = this;
		return globalInfo.TrueValue;
	}
}
