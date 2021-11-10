// clu/domain-object-model/selector-definition.ts

import {
	ICLUEnvironmentFrame,
	ICLUGlobalInfo,
	ICluster,
	ICLUValue,
	ICLUVariable
} from './interfaces/ivalue';

import { CLUFunctionDefinitionBase } from './function-definition-base';

const typenameCLUSelectorDefinition = 'CLUSelectorDefinition';

export function isCLUSelectorDefinition(obj: unknown): obj is CLUSelectorDefinition {
	const v = obj as CLUSelectorDefinition;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameCLUSelectorDefinition
	);
}

export class CLUSelectorDefinition extends CLUFunctionDefinitionBase {
	public readonly typename: string = typenameCLUSelectorDefinition;
	// public readonly CLUVariable AssociatedVariable;

	constructor(funcName: string, public readonly associatedVariable: ICLUVariable) {
		super(funcName);

		// AssociatedVariable = associatedVariable;
	}

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster | undefined,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		return localEnvironment.lookup(this.associatedVariable);
	}
}
