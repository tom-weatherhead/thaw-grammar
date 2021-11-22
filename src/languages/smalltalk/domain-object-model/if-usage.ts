// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/if-usage.ts

import {
	// ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	ISmalltalkGlobalInfo,
	ISmalltalkValue
} from './interfaces/iexpression';

import { unblockValue } from './data-types/block';

export class SmalltalkIfUsage implements ISmalltalkExpression {
	constructor(
		public readonly condition: ISmalltalkExpression,
		public readonly ifBody: ISmalltalkExpression,
		public readonly elseBody: ISmalltalkExpression
	) {}

	public evaluate(
		globalInfo: ISmalltalkGlobalInfo, // I.e. IGlobalInfo<ISmalltalkValue>
		localEnvironment: ISmalltalkEnvironmentFrame | undefined, // I.e. IEnvironmentFrame<ISmalltalkValue> | undefined
		options?: unknown
	): ISmalltalkValue {
		const conditionValue = unblockValue(
			this.condition.evaluate(globalInfo, localEnvironment, options)
		);

		if (!globalInfo.valueIsFalse(conditionValue)) {
			return this.ifBody.evaluate(globalInfo, localEnvironment, options);
		} else {
			return this.elseBody.evaluate(globalInfo, localEnvironment, options);
		}
	}
}
