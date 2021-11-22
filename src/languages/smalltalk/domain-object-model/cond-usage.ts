// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/cond-usage.ts

import {
	// ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	// ISmalltalkFunctionDefinition,
	ISmalltalkGlobalInfo,
	ISmalltalkValue // ,
	// ISmalltalkVariable
} from './interfaces/iexpression';

import { unblockValue } from './data-types/block';

export class SmalltalkCondUsage implements ISmalltalkExpression {
	constructor(public readonly exprPairList: [ISmalltalkExpression, ISmalltalkExpression][]) {}

	public evaluate(
		globalInfo: ISmalltalkGlobalInfo, // I.e. IGlobalInfo<ISmalltalkValue>
		localEnvironment: ISmalltalkEnvironmentFrame | undefined, // I.e. IEnvironmentFrame<ISmalltalkValue> | undefined
		options?: unknown
	): ISmalltalkValue {
		for (const [key, value] of this.exprPairList) {
			if (
				!globalInfo.valueIsFalse(
					unblockValue(key.evaluate(globalInfo, localEnvironment, options))
				)
			) {
				return value.evaluate(globalInfo, localEnvironment, options);
			}
		}

		return globalInfo.falseValue;
	}
}
