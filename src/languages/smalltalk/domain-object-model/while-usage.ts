// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/while-usage.ts

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	ISmalltalkGlobalInfo,
	ISmalltalkValue
} from './interfaces/iexpression';

import { unblockValue } from './block';

export class SmalltalkWhileUsage implements ISmalltalkExpression {
	constructor(
		public readonly condition: ISmalltalkExpression,
		public readonly body: ISmalltalkExpression
	) {}

	/*
    public override string ToString()
    {
        return string.Format("(while {0} {1})", Condition, Body);
    }
     */

	public evaluate(
		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		receiver: ISmalltalkValue, // | undefined,
		c: ISmalltalkClass | undefined,
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue {
		// #if DEAD_CODE
		// ISmalltalkValue conditionValue;
		// ISmalltalkValue falseValue = globalInfo.FalseValue;
		//
		// for (; ; )
		// {
		//     conditionValue = Condition.Evaluate(localEnvironment, receiver, c, globalInfo);
		//
		//     if (conditionValue.Equals(falseValue))
		//     {
		//         break;
		//     }
		//
		//     Body.Evaluate(localEnvironment, receiver, c, globalInfo);
		// }
		//
		// return conditionValue;
		// #else

		while (
			!globalInfo.valueIsFalse(
				unblockValue(this.condition.evaluate(localEnvironment, receiver, c, globalInfo))
			)
		) {
			this.body.evaluate(localEnvironment, receiver, c, globalInfo);
		}

		return globalInfo.falseValue;
		// #endif
	}
}
