// clu/domain-object-model/settor-definition.ts

import { Name } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';

import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import {
	// ICLUEnvironmentFrame,
	// ICLUGlobalInfo,
	// ICluster,
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
	public setValue: ICLUValue | undefined; // This must be set before Evaluate() is called.

	constructor(funcName: Name, public readonly associatedVariable: ICLUVariable) {
		super(funcName);
	}

	// public evaluate(
	// 	localEnvironment: ICLUEnvironmentFrame,
	// 	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	// 	cluster: ICluster | undefined,
	// 	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	// 	globalInfo: ICLUGlobalInfo
	// ): ICLUValue {
	public evaluate(
		globalInfo: IGlobalInfo<ICLUValue>,
		localEnvironment?: IEnvironmentFrame<ICLUValue>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): ICLUValue {
		if (typeof this.setValue === 'undefined') {
			throw new Error('CLUSettorDefinition.evaluate() : this.setValue is undefined');
		} else if (typeof localEnvironment === 'undefined') {
			throw new Error('CLUSettorDefinition.evaluate() : localEnvironment is undefined');
		}

		localEnvironment.add(this.associatedVariable, this.setValue);

		return this.setValue;
	}
}
