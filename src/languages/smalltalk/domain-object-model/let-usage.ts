// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/let-usage.ts

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	// ISmalltalkFunctionDefinition,
	ISmalltalkGlobalInfo,
	ISmalltalkValue,
	ISmalltalkVariable
} from './interfaces/iexpression';

// import { unblockValue } from './block';

import { SmalltalkEnvironmentFrame } from './environment-frame';

export class SmalltalkLetUsage implements ISmalltalkExpression {
	// public readonly List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>> Bindings;
	// public readonly ISmalltalkExpression Expression;

	constructor(
		public readonly bindings: [ISmalltalkVariable, ISmalltalkExpression][],
		public readonly expression: ISmalltalkExpression
	) {
		// Bindings = bindings;
		// Expression = expression;
	}

	public evaluate(
		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		receiver: ISmalltalkValue | undefined,
		c: ISmalltalkClass | undefined,
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue {
		const newEnvFrame = new SmalltalkEnvironmentFrame(localEnvironment);

		for (const [key, value] of this.bindings) {
			newEnvFrame.add(key, value.evaluate(localEnvironment, receiver, c, globalInfo));
		}

		return this.expression.evaluate(newEnvFrame, receiver, c, globalInfo);
	}
}
