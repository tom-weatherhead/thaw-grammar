// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/begin-usage.ts

// import {
// 	// ISmalltalkClass,
// 	ISmalltalkEnvironmentFrame,
// 	ISmalltalkExpression,
// 	ISmalltalkGlobalInfo,
// 	ISmalltalkValue
// } from './interfaces/iexpression';
//
// export class SmalltalkBeginUsage implements ISmalltalkExpression {
// 	constructor(
// 		public readonly firstExpression: ISmalltalkExpression,
// 		public readonly expressionList: ISmalltalkExpression[]
// 	) {}
//
// 	public evaluate(
// 		globalInfo: ISmalltalkGlobalInfo, // I.e. IGlobalInfo<ISmalltalkValue>
// 		localEnvironment: ISmalltalkEnvironmentFrame | undefined, // I.e. IEnvironmentFrame<ISmalltalkValue> | undefined
// 		options?: unknown
// 	): ISmalltalkValue {
// 		let result = this.firstExpression.evaluate(globalInfo, localEnvironment, options);
//
// 		for (const expression of this.expressionList) {
// 			result = expression.evaluate(globalInfo, localEnvironment, options);
// 		}
//
// 		return result;
// 	}
// }
