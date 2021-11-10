// clu/domain-object-model/constructor-definition.ts

import { ICLUEnvironmentFrame, ICLUGlobalInfo, ICluster, ICLUValue } from './interfaces/ivalue';

import { CLUUserValue } from './data-types/user-value';

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
	//public readonly string ClusterName;

	constructor(/* string funcName, */ clusterName: string) {
		super(clusterName);

		//ClusterName = clusterName;
	}

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
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
		return new CLUUserValue(cluster, localEnvironment);
	}
}
