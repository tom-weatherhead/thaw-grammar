// clu/domain-object-model/settor-definition.ts

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUGlobalInfo,
	ICluster,
	ICLUValue,
	ICLUVariable
} from './interfaces/ivalue';

import { CLUFunctionDefinitionBase } from './function-definition-base';

const typenameCLUSettorDefinition = 'CLUSettorDefinition';

export function isCLUSettorDefinition(obj: unknown): obj is CLUSettorDefinition {
	const v = obj as CLUSettorDefinition;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameCLUSettorDefinition
	);
}

export class CLUSettorDefinition extends CLUFunctionDefinitionBase {
	public readonly typename: string = typenameCLUSettorDefinition;
	//public readonly string SelName;
	// public readonly CLUVariable AssociatedVariable;
	public setValue: ICLUValue | undefined; // This must be set before Evaluate() is called.

	constructor(
		funcName: string,
		/* string selName, */ public readonly associatedVariable: ICLUVariable
	) {
		super(funcName);

		//SelName = selName;
		// AssociatedVariable = associatedVariable;
	}

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster | undefined,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		localEnvironment.add(this.associatedVariable, this.setValue);

		return this.setValue;
	}
}
