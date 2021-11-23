// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/let-usage.ts

// import {
// 	// ISmalltalkClass,
// 	ISmalltalkEnvironmentFrame,
// 	ISmalltalkExpression,
// 	ISmalltalkGlobalInfo,
// 	ISmalltalkValue,
// 	ISmalltalkVariable
// } from './interfaces/iexpression';
//
// import { SmalltalkEnvironmentFrame } from './environment-frame';
//
// export class SmalltalkLetUsage implements ISmalltalkExpression {
// 	constructor(
// 		public readonly bindings: [ISmalltalkVariable, ISmalltalkExpression][],
// 		public readonly expression: ISmalltalkExpression
// 	) {}
//
// 	public evaluate(
// 		globalInfo: ISmalltalkGlobalInfo, // I.e. IGlobalInfo<ISmalltalkValue>
// 		localEnvironment: ISmalltalkEnvironmentFrame | undefined, // I.e. IEnvironmentFrame<ISmalltalkValue> | undefined
// 		options?: unknown
// 	): ISmalltalkValue {
// 		const newEnvFrame = new SmalltalkEnvironmentFrame(localEnvironment);
//
// 		for (const [key, value] of this.bindings) {
// 			newEnvFrame.add(key, value.evaluate(globalInfo, localEnvironment, options));
// 		}
//
// 		return this.expression.evaluate(globalInfo, newEnvFrame, options);
// 	}
// }
