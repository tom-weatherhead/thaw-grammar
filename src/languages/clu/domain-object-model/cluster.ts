// clu/domain-object-model/cluster.ts

import { Name } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';

import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import {
	// ICLUEnvironmentFrame,
	ICluEvaluateOptions,
	// ICLUExpression,
	ICLUFunctionDefinition,
	// ICLUGlobalInfo,
	ICluster,
	ICLUValue,
	ICLUVariable
} from './interfaces/ivalue';

import { CLUConstructorDefinition } from './constructor-definition';

import { CLUFunctionDefinitionBase } from './function-definition-base';

import { isCLUGlobalInfo } from './global-info';

import { CLUSelectorDefinition } from './selector-definition';

import { CLUSettorDefinition } from './settor-definition';

const typenameCluster = 'Cluster';

export function isCluster(obj: unknown): obj is ICluster {
	const other = obj as Cluster;

	return typeof other !== 'undefined' && other.typename === typenameCluster;
}

export function isCluEvaluateOptions(obj: unknown): obj is ICluEvaluateOptions {
	const other = obj as ICluEvaluateOptions;

	return (
		typeof other !== 'undefined' &&
		(typeof other.cluster === 'undefined' || isCluster(other.cluster))
	);
}

export class Cluster implements ICluster {
	public readonly typename: string = typenameCluster;
	public readonly exportedDict = new Map<string, ICLUFunctionDefinition>();
	public readonly nonExportedDict = new Map<string, ICLUFunctionDefinition>();

	constructor(
		public readonly clusterName: Name,
		public readonly exportSet: string[],
		public readonly clRep: ICLUVariable[],
		private funDefList: CLUFunctionDefinitionBase[]
	) {}

	// public evaluate(
	// 	localEnvironment: ICLUEnvironmentFrame,
	// 	cluster: ICluster | undefined,
	// 	globalInfo: ICLUGlobalInfo
	// ): ICLUValue {
	public evaluate(
		globalInfo: IGlobalInfo<ICLUValue>,
		localEnvironment?: IEnvironmentFrame<ICLUValue>,
		options?: unknown
	): ICLUValue {
		if (!isCLUGlobalInfo(globalInfo)) {
			throw new Error('Cluster.evaluate() : globalInfo is not isCLUGlobalInfo.');
		}

		globalInfo.clusterDict.set(this.clusterName.value, this);

		// Make the constructor:
		this.nonExportedDict.set(
			this.clusterName.value,
			new CLUConstructorDefinition(this.clusterName)
		);

		// Make the selectors and settors:

		for (const memberVariable of this.clRep) {
			this.nonExportedDict.set(
				memberVariable.name,
				new CLUSelectorDefinition(new Name(memberVariable.name), memberVariable)
			);
			this.nonExportedDict.set(
				'set-' + memberVariable.name,
				new CLUSettorDefinition(new Name('set-' + memberVariable.name), memberVariable)
			);
		}

		for (const funDef of this.funDefList) {
			if (this.exportSet.indexOf(funDef.functionName.value) >= 0) {
				this.exportedDict.set(funDef.functionName.value, funDef);
			} else {
				this.nonExportedDict.set(funDef.functionName.value, funDef);
			}
		}

		return globalInfo.trueValue;
	}
}
