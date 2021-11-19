// clu/domain-object-model/let-usage.ts

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';

import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import {
	// ICLUEnvironmentFrame,
	ICLUExpression,
	// ICLUGlobalInfo,
	// ICluster,
	ICLUValue,
	ICLUVariable
} from './interfaces/ivalue';

import { CLUEnvironmentFrame } from './environment-frame';

export class CLULetUsage implements ICLUExpression {
	constructor(
		public readonly bindings: [ICLUVariable, ICLUExpression][],
		public readonly expression: ICLUExpression
	) {}

	// public evaluate(
	// 	localEnvironment: ICLUEnvironmentFrame,
	// 	cluster: ICluster | undefined,
	// 	globalInfo: ICLUGlobalInfo
	// ): ICLUValue {
	public evaluate(
		globalInfo: IGlobalInfo<ICLUValue>,
		localEnvironment?: IEnvironmentFrame<ICLUValue>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): ICLUValue {
		const newEnvFrame = new CLUEnvironmentFrame(localEnvironment);

		for (const [key, value] of this.bindings) {
			newEnvFrame.add(key, value.evaluate(globalInfo, localEnvironment, options));
		}

		return this.expression.evaluate(globalInfo, newEnvFrame, options);
	}
}
