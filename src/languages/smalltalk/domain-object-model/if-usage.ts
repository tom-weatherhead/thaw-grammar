// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/if-usage.ts

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

export class SmalltalkIfUsage implements ISmalltalkExpression {
	// public readonly ISmalltalkExpression Condition;
	// public readonly ISmalltalkExpression IfBody;
	// public readonly ISmalltalkExpression ElseBody;

	constructor(
		public readonly condition: ISmalltalkExpression,
		public readonly ifBody: ISmalltalkExpression,
		public readonly elseBody: ISmalltalkExpression
	) {
		// Condition = condition;
		// IfBody = ifBody;
		// ElseBody = elseBody;
	}

	/*
    public override string ToString()
    {
        return string.Format("(if {0} {1} {2})", Condition, IfBody, ElseBody);
    }
     */

	public evaluate(
		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		receiver: ISmalltalkValue | undefined,
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
