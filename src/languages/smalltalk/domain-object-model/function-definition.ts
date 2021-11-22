// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/function-definition.ts

import { Name } from 'thaw-interpreter-core';

import {
	// ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	ISmalltalkFunctionDefinition,
	ISmalltalkGlobalInfo,
	ISmalltalkValue,
	ISmalltalkVariable
} from './interfaces/iexpression';

export class SmalltalkFunctionDefinition implements ISmalltalkFunctionDefinition {
	constructor(
		public readonly functionName: Name,
		public readonly argList: ISmalltalkVariable[],
		public readonly body: ISmalltalkExpression
	) {}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public evaluate(
		globalInfo: ISmalltalkGlobalInfo, // I.e. IGlobalInfo<ISmalltalkValue>
		localEnvironment: ISmalltalkEnvironmentFrame | undefined, // I.e. IEnvironmentFrame<ISmalltalkValue> | undefined
		options?: unknown
	): ISmalltalkValue {
		globalInfo.functionDefinitions.set(this.functionName.value, this);

		return globalInfo.falseValue;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
