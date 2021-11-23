// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/while-usage.ts

// import {
// 	// ISmalltalkClass,
// 	ISmalltalkEnvironmentFrame,
// 	ISmalltalkExpression,
// 	ISmalltalkGlobalInfo,
// 	ISmalltalkValue
// } from './interfaces/iexpression';
//
// import { unblockValue } from './data-types/block';
//
// export class SmalltalkWhileUsage implements ISmalltalkExpression {
// 	constructor(
// 		public readonly condition: ISmalltalkExpression,
// 		public readonly body: ISmalltalkExpression
// 	) {}
//
// 	public evaluate(
// 		globalInfo: ISmalltalkGlobalInfo, // I.e. IGlobalInfo<ISmalltalkValue>
// 		localEnvironment: ISmalltalkEnvironmentFrame | undefined, // I.e. IEnvironmentFrame<ISmalltalkValue> | undefined
// 		options?: unknown
// 	): ISmalltalkValue {
// 		// #if DEAD_CODE
// 		// ISmalltalkValue conditionValue;
// 		// ISmalltalkValue falseValue = globalInfo.FalseValue;
// 		//
// 		// for (; ; )
// 		// {
// 		//     conditionValue = Condition.Evaluate(localEnvironment, receiver, c, globalInfo);
// 		//
// 		//     if (conditionValue.Equals(falseValue))
// 		//     {
// 		//         break;
// 		//     }
// 		//
// 		//     Body.Evaluate(localEnvironment, receiver, c, globalInfo);
// 		// }
// 		//
// 		// return conditionValue;
// 		// #else
//
// 		while (
// 			!globalInfo.valueIsFalse(
// 				unblockValue(this.condition.evaluate(globalInfo, localEnvironment, options))
// 			)
// 		) {
// 			this.body.evaluate(globalInfo, localEnvironment, options);
// 		}
//
// 		return globalInfo.falseValue;
// 		// #endif
// 	}
// }
