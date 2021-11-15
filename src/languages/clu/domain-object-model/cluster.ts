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
	public readonly exportedDict = new Map<string, CLUFunctionDefinitionBase>();
	public readonly nonExportedDict = new Map<string, CLUFunctionDefinitionBase>();

	constructor(
		public readonly clusterName: string,
		public readonly exportSet: string[],
		public readonly clRep: ICLUVariable[],
		private funDefList: CLUFunctionDefinitionBase[]
	) {}

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster | undefined,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		globalInfo.clusterDict.set(this.clusterName, this);

		// Make the constructor:
		this.nonExportedDict.set(this.clusterName, new CLUConstructorDefinition(this.clusterName));

		// Make the selectors and settors:

		for (const memberVariable of this.clRep) {
			this.nonExportedDict.set(
				memberVariable.name,
				new CLUSelectorDefinition(memberVariable.name, memberVariable)
			);
			this.nonExportedDict.set(
				'set-' + memberVariable.name,
				new CLUSettorDefinition('set-' + memberVariable.name, memberVariable)
			);
		}

		for (const funDef of this.funDefList) {
			if (this.exportSet.indexOf(funDef.functionName) >= 0) {
				this.exportedDict.set(funDef.functionName, funDef);
			} else {
				this.nonExportedDict.set(funDef.functionName, funDef);
			}
		}

		return globalInfo.trueValue;
	}
}
