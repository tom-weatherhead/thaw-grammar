// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/function-definition.ts

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	ISmalltalkFunctionDefinition,
	ISmalltalkGlobalInfo,
	ISmalltalkValue,
	ISmalltalkVariable
} from './interfaces/iexpression';

export class SmalltalkFunctionDefinition implements ISmalltalkFunctionDefinition {
	constructor(
		public readonly functionName: string,
		public readonly argList: ISmalltalkVariable[],
		public readonly body: ISmalltalkExpression
	) {}

	/*
    public override string ToString()
    {
        return string.Format("(define {0} {1} {2})", FunctionName, ArgList, Body);
    }
     */

	/* eslint-disable no-unused-vars */
	public evaluate(
		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		receiver: ISmalltalkValue, // | undefined,
		c: ISmalltalkClass | undefined,
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue {
		globalInfo.functionDefinitions.set(this.functionName, this);

		return globalInfo.falseValue;
	}
	/* eslint-enable no-unused-vars */
}
