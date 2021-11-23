// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/set-usage.ts

import {
	ISmalltalkEnvironmentFrame,
	ISmalltalkEvaluateOptions,
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

	public evaluate(
		globalInfo: ISmalltalkGlobalInfo, // I.e. IGlobalInfo<ISmalltalkValue>
		localEnvironment: ISmalltalkEnvironmentFrame | undefined, // I.e. IEnvironmentFrame<ISmalltalkValue> | undefined
		options?: unknown
	): ISmalltalkValue {
		if (typeof options === 'undefined') {
			throw new Error('SmalltalkVariable.evaluate() : options is undefined');
		}

		const optionsX = options as ISmalltalkEvaluateOptions;
		const c = optionsX.c;
		const receiver = optionsX.receiver;

		const expressionValue = unblockValue(
			this.expression.evaluate(globalInfo, localEnvironment, options)
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
