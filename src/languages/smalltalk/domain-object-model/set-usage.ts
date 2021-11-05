// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/set-usage.ts

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	ISmalltalkGlobalInfo,
	ISmalltalkValue,
	ISmalltalkVariable
} from './interfaces/iexpression';

import { unblockValue } from './data-types/block';

export class SmalltalkSetUsage implements ISmalltalkExpression {
	constructor(
		public readonly variableName: ISmalltalkVariable,
		public readonly expression: ISmalltalkExpression
	) {}

	/*
    public override string ToString()
    {
        return string.Format("(set {0} {1})", VariableName, Expression);
    }
     */

	public evaluate(
		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		receiver: ISmalltalkValue, // | undefined,
		c: ISmalltalkClass | undefined,
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue {
		const expressionValue = unblockValue(
			this.expression.evaluate(localEnvironment, receiver, c, globalInfo)
		);
		const userVal = typeof receiver !== 'undefined' ? receiver.toUserValue() : undefined;

		if (
			typeof localEnvironment !== 'undefined' &&
			localEnvironment.isDefined(this.variableName)
		) {
			localEnvironment.addBubbleDown(this.variableName, expressionValue);
		} else if (
			typeof userVal !== 'undefined' &&
			userVal.value.dict.has(this.variableName.name)
		) {
			userVal.value.dict.set(this.variableName.name, expressionValue);
		} else if (
			typeof c === 'undefined' ||
			!c.trySetClassVariableValue(this.variableName, expressionValue)
		) {
			globalInfo.globalEnvironment.dict.set(this.variableName.name, expressionValue);
		}

		return expressionValue;
	}
}
