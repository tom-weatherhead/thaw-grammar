// clu/domain-object-model/selector-definition.ts

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
		options?: unknown
	): ICLUValue {
		if (typeof localEnvironment === 'undefined') {
			throw new Error('CLUSelectorDefinition.evaluate() : localEnvironment is undefined');
		}

		return localEnvironment.lookup(this.associatedVariable);
	}
}
