// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/begin-usage.ts

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	ISmalltalkGlobalInfo,
	ISmalltalkValue
} from './interfaces/iexpression';

export class SmalltalkBeginUsage implements ISmalltalkExpression {
	constructor(
		public readonly firstExpression: ISmalltalkExpression,
		public readonly expressionList: ISmalltalkExpression[]
	) {}

	/*
    public override string ToString()
    {
        return string.Format("(begin {0} {1})", FirstExpression, ExpressionList);
    }
     */

	public evaluate(
		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		receiver: ISmalltalkValue, // | undefined,
		c: ISmalltalkClass | undefined,
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue {
		let result = this.firstExpression.evaluate(localEnvironment, receiver, c, globalInfo);

		for (const expression of this.expressionList) {
			result = expression.evaluate(localEnvironment, receiver, c, globalInfo);
		}

		return result;
	}
}
