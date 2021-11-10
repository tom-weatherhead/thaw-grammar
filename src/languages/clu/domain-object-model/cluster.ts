// clu/domain-object-model/cluster.ts

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUGlobalInfo,
	ICluster,
	ICLUValue,
	ICLUVariable
} from './interfaces/ivalue';

import { CLUConstructorDefinition } from './constructor-definition';

import { CLUFunctionDefinitionBase } from './function-definition-base';

import { CLUSelectorDefinition } from './selector-definition';

import { CLUSettorDefinition } from './settor-definition';

export class Cluster implements ICLUExpression {
	// public readonly clusterName: string;
	// public readonly exportSet: string[]; // HashSet<string>;
	// public readonly clRep: ICLUVariable[];
	// private funDefList: CLUFunctionDefinitionBase[];
	public readonly exportedDict = new Map<string, CLUFunctionDefinitionBase>();
	public readonly nonExportedDict = new Map<string, CLUFunctionDefinitionBase>();

	constructor(
		public readonly clusterName: string,
		public readonly exportSet: string[],
		public readonly clRep: ICLUVariable[],
		private funDefList: CLUFunctionDefinitionBase[]
	) {
		// ClusterName = name;
		// ExportSet = exportSet;
		// ClRep = clRep;
		// FunDefList = funDefList;
	}

	// public override bool Equals(object obj)
	// {
	//
	// 	if (object.ReferenceEquals(this, obj))
	// 	{
	// 		return true;
	// 	}
	//
	// 	var otherCluster = obj as Cluster;
	//
	// 	return otherCluster != null && ClusterName == otherCluster.ClusterName;
	// }
	//
	// public override int GetHashCode()
	// {
	// 	return ClusterName.GetHashCode();
	// }

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		globalInfo.clusterDict.set(this.clusterName, this);

		// Make the constructor:
		this.nonExportedDict[this.clusterName] = new CLUConstructorDefinition(this.clusterName);

		// Make the selectors and settors:

		for (const memberVariable of this.clRep) {
			this.nonExportedDict[memberVariable.name] = new CLUSelectorDefinition(
				memberVariable.name,
				memberVariable
			);
			this.nonExportedDict['set-' + memberVariable.name] = new CLUSettorDefinition(
				'set-' + memberVariable.name,
				memberVariable
			);
		}

		for (const funDef of this.funDefList) {
			if (this.exportSet.indexOf(funDef.functionName) >= 0) {
				this.exportedDict[funDef.functionName] = funDef;
			} else {
				this.nonExportedDict[funDef.functionName] = funDef;
			}
		}

		return globalInfo.trueValue;
	}
}
