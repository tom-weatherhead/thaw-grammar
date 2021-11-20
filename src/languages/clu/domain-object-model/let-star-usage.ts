// clu/domain-object-model/let-star-usage.ts

import {
	EnvironmentFrame,
	IEnvironmentFrame
} from '../../../common/domain-object-model/environment-frame';

import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import {
	// ICLUEnvironmentFrame,
	ICLUExpression,
	// ICLUGlobalInfo,
	// ICluster,
	ICLUValue,
	ICLUVariable
} from './interfaces/ivalue';

// import { CLUEnvironmentFrame } from './environment-frame';

export class CLULetStarUsage implements ICLUExpression {
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
		// #if DEAD_CODE
		// var newEnvFrame = new CLUEnvironmentFrame(localEnvironment);
		//
		// foreach (var binding in Bindings)
		// {
		// 	newEnvFrame.Add(binding.Key, binding.Value.Evaluate(newEnvFrame, cluster, globalInfo));
		// }
		//
		// return Expression.Evaluate(newEnvFrame, cluster, globalInfo);
		// #else

		// 2014/02/17 : This implementation does not support recursive definitions.
		let lastEnv = localEnvironment;

		for (const [key, value] of this.bindings) {
			const newEnvFrame = new EnvironmentFrame<ICLUValue>(lastEnv);

			newEnvFrame.add(key, value.evaluate(globalInfo, lastEnv, options));
			lastEnv = newEnvFrame;
		}

		return this.expression.evaluate(globalInfo, lastEnv, options);
		// #endif
	}
}
