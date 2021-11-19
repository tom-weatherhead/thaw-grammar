// clu/domain-object-model/constructor-definition.ts

import { Name } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';

import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import {
	/* ICLUEnvironmentFrame, ICLUGlobalInfo, ICluster, */ ICLUValue
} from './interfaces/ivalue';

import { CLUUserValue } from './data-types/user-value';

import { isCluEvaluateOptions } from './cluster';

import { CLUFunctionDefinitionBase } from './function-definition-base';

const typenameCLUConstructorDefinition = 'CLUConstructorDefinition';

export function isCLUConstructorDefinition(obj: unknown): obj is CLUConstructorDefinition {
	const v = obj as CLUConstructorDefinition;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameCLUConstructorDefinition
	);
}

export class CLUConstructorDefinition extends CLUFunctionDefinitionBase {
	public readonly typename: string = typenameCLUConstructorDefinition;

	constructor(/* string funcName, */ clusterName: Name) {
		super(clusterName);
	}

	// public evaluate(
	// 	localEnvironment: ICLUEnvironmentFrame,
	// 	cluster: ICluster | undefined,
	// 	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	// 	globalInfo: ICLUGlobalInfo
	// ): ICLUValue {
	public evaluate(
		globalInfo: IGlobalInfo<ICLUValue>,
		localEnvironment?: IEnvironmentFrame<ICLUValue>,
		options?: unknown
	): ICLUValue {
		// if (typeof options === 'undefined' || !isCluster(options.cluster)) {
		// 	throw new Error('CLUConstructorDefinition.evaluate() : cluster is undefined');
		if (!isCluEvaluateOptions(options)) {
			throw new Error(
				'CLUConstructorDefinition.evaluate() : options is not CluEvaluateOptions'
			);
		} else if (typeof options.cluster === 'undefined') {
			throw new Error('CLUConstructorDefinition.evaluate() : options.cluster is undefined');
		} else if (typeof localEnvironment === 'undefined') {
			throw new Error('CLUConstructorDefinition.evaluate() : localEnvironment is undefined');
		}

		/*
		// ClusterName is the name of the cluster of which newInstance is an instance.

		if (!globalInfo.ClusterDict.ContainsKey(ClusterName))
		{
			throw new Exception(string.Format("CLUConstructorDefinition.Evaluate() : Unknown cluster '{0}'.", FunctionName));
		}

		var newInstance = new CLUUserValue(localEnvironment);
		var zero = new CLUPrimitiveValue(0);    // This can be shared because it is immutable.

		foreach (var variable in globalInfo.ClusterDict[ClusterName].ClRep)
		{
			newInstance.Value.Dict[variable] = zero;
		}

		return newInstance;
		 */
		return new CLUUserValue(options.cluster, localEnvironment);
	}
}
