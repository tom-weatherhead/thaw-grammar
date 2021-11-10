// clu/domain-object-model/selector-definition.ts

import { ICLUEnvironmentFrame, ICLUExpression, ICLUGlobalInfo, ICluster, ICLUValue } from './interfaces/ivalue';

export class CLUSelectorDefinition extends CLUFunctionDefinitionBase {
	public readonly CLUVariable AssociatedVariable;

	constructor(string funcName, CLUVariable associatedVariable)
		: base(funcName)
	{
		AssociatedVariable = associatedVariable;
	}

	public evaluate(localEnvironment: ICLUEnvironmentFrame, cluster: ICluster, globalInfo: ICLUGlobalInfo): ICLUValue {
		return localEnvironment.Dict[AssociatedVariable];
	}
}
