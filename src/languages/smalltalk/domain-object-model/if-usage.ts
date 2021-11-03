// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/if-usage.ts

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	ISmalltalkGlobalInfo,
	ISmalltalkValue
} from './interfaces/iexpression';

import { unblockValue } from './block';

export class SmalltalkIfUsage implements ISmalltalkExpression {
	constructor(
		public readonly condition: ISmalltalkExpression,
		public readonly ifBody: ISmalltalkExpression,
		public readonly elseBody: ISmalltalkExpression
	) {}

	/*
    public override string ToString()
    {
        return string.Format("(if {0} {1} {2})", Condition, IfBody, ElseBody);
    }
     */

	public evaluate(
		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		receiver: ISmalltalkValue, // | undefined,
		c: ISmalltalkClass | undefined,
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue {
		const conditionValue = unblockValue(
			this.condition.evaluate(localEnvironment, receiver, c, globalInfo)
		);

		if (!globalInfo.valueIsFalse(conditionValue)) {
			return this.ifBody.evaluate(localEnvironment, receiver, c, globalInfo);
		} else {
			return this.elseBody.evaluate(localEnvironment, receiver, c, globalInfo);
		}
	}
}
