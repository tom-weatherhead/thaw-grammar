// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/let-star-usage.ts

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
// export class SmalltalkLetStarUsage implements ISmalltalkExpression {
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
// 		// #if DEAD_CODE
// 		// var newEnvFrame = new SmalltalkEnvironmentFrame(localEnvironment);
// 		//
// 		// foreach (var binding in Bindings)
// 		// {
// 		//     newEnvFrame.Add(binding.Key, binding.Value.Evaluate(newEnvFrame, receiver, c, globalInfo));
// 		// }
// 		//
// 		// return Expression.Evaluate(newEnvFrame, receiver, c, globalInfo);
// 		// #else
//
// 		// 2014/02/17 : This implementation does not support recursive definitions.
// 		let lastEnv = localEnvironment;
//
// 		for (const [key, value] of this.bindings) {
// 			const newEnvFrame = new SmalltalkEnvironmentFrame(lastEnv);
//
// 			newEnvFrame.add(key, value.evaluate(globalInfo, lastEnv, options));
// 			lastEnv = newEnvFrame;
// 		}
//
// 		return this.expression.evaluate(globalInfo, lastEnv, options);
// 		// #endif
// 	}
// }
