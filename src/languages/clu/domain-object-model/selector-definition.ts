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

	constructor(funcName: string, public readonly associatedVariable: ICLUVariable) {
		super(funcName);
	}

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		cluster: ICluster | undefined,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		return localEnvironment.lookup(this.associatedVariable);
	}
}
