// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/cond-usage.ts

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

export class SmalltalkCondUsage implements ISmalltalkExpression {
	// public readonly List<KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>> ExprPairList;

	constructor(public readonly exprPairList: [ISmalltalkExpression, ISmalltalkExpression][]) {
		// ExprPairList = exprPairList;
	}

	public evaluate(
		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		receiver: ISmalltalkValue | undefined,
		c: ISmalltalkClass | undefined,
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue {
		for (const [key, value] of this.exprPairList) {
			//if (!exprPair.Key.Evaluate(localEnvironment, receiver, c, globalInfo).Equals(falseValue))
			if (
				!globalInfo.valueIsFalse(
					unblockValue(key.evaluate(localEnvironment, receiver, c, globalInfo))
				)
			) {
				return value.evaluate(localEnvironment, receiver, c, globalInfo);
			}
		}

		//return falseValue;
		return globalInfo.zeroValue;
	}
}
