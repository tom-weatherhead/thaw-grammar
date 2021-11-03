// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/while-usage.ts

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	// ISmalltalkFunctionDefinition,
	ISmalltalkGlobalInfo,
	ISmalltalkValue // ,
	// ISmalltalkVariable
} from './interfaces/iexpression';

import { unblockValue } from './block';

export class SmalltalkWhileUsage implements ISmalltalkExpression {
	// public readonly ISmalltalkExpression Condition;
	// public readonly ISmalltalkExpression Body;

	constructor(
		public readonly condition: ISmalltalkExpression,
		public readonly body: ISmalltalkExpression
	) {
		// Condition = condition;
		// Body = body;
	}

	/*
    public override string ToString()
    {
        return string.Format("(while {0} {1})", Condition, Body);
    }
     */

	public evaluate(
		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		receiver: ISmalltalkValue | undefined,
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

		return globalInfo.zeroValue;
		// #endif
	}
}
