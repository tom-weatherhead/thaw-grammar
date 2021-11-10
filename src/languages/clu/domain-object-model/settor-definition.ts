// clu/domain-object-model/settor-definition.ts

import { ICLUEnvironmentFrame, ICLUExpression, ICLUGlobalInfo, ICluster, ICLUValue } from './interfaces/ivalue';

export class CLUSettorDefinition extends CLUFunctionDefinitionBase {
	//public readonly string SelName;
	public readonly CLUVariable AssociatedVariable;
	public ICLUValue SetValue;  // This must be set before Evaluate() is called.

	constructor(string funcName, /* string selName, */ CLUVariable associatedVariable)
		: base(funcName)
	{
		//SelName = selName;
		AssociatedVariable = associatedVariable;
	}

	public evaluate(localEnvironment: ICLUEnvironmentFrame, cluster: ICluster, globalInfo: ICLUGlobalInfo): ICLUValue {
		localEnvironment.Dict[AssociatedVariable] = SetValue;
		return SetValue;
	}
}
