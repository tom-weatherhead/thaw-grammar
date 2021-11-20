// clu/domain-object-model/let-usage.ts

// import {
// 	EnvironmentFrame,
// 	IEnvironmentFrame
// } from '../../../common/domain-object-model/environment-frame';
//
// import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
//
// import {
// 	ICLUExpression,
// 	ICLUValue,
// 	ICLUVariable
// } from './interfaces/ivalue';
//
// export class CLULetUsage implements ICLUExpression {
// 	constructor(
// 		public readonly bindings: [ICLUVariable, ICLUExpression][],
// 		public readonly expression: ICLUExpression
// 	) {}
//
// 	public evaluate(
// 		globalInfo: IGlobalInfo<ICLUValue>,
// 		localEnvironment?: IEnvironmentFrame<ICLUValue>,
// 		options?: unknown
// 	): ICLUValue {
// 		const newEnvFrame = new EnvironmentFrame<ICLUValue>(localEnvironment);
//
// 		for (const [key, value] of this.bindings) {
// 			newEnvFrame.add(key, value.evaluate(globalInfo, localEnvironment, options));
// 		}
//
// 		return this.expression.evaluate(globalInfo, newEnvFrame, options);
// 	}
// }
