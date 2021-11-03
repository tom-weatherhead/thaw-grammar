// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/let-star-usage.ts

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

export class SmalltalkLetStarUsage implements ISmalltalkExpression {
	// public readonly List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>> Bindings;
	// public readonly ISmalltalkExpression Expression;

	// public SmalltalkLetStarUsage(List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>> bindings, ISmalltalkExpression expression)
	// {
	//     Bindings = bindings;
	//     Expression = expression;
	// }

	constructor(
		public readonly bindings: [ISmalltalkVariable, ISmalltalkExpression][],
		public readonly expression: ISmalltalkExpression
	) {
		// Bindings = bindings;
		// Expression = expression;
	}

	public evaluate(
		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		receiver: ISmalltalkValue, // | undefined,
		c: ISmalltalkClass | undefined,
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue {
		// #if DEAD_CODE
		// var newEnvFrame = new SmalltalkEnvironmentFrame(localEnvironment);
		//
		// foreach (var binding in Bindings)
		// {
		//     newEnvFrame.Add(binding.Key, binding.Value.Evaluate(newEnvFrame, receiver, c, globalInfo));
		// }
		//
		// return Expression.Evaluate(newEnvFrame, receiver, c, globalInfo);
		// #else

		// 2014/02/17 : This implementation does not support recursive definitions.
		let lastEnv = localEnvironment;

		for (const [key, value] of this.bindings) {
			const newEnvFrame = new SmalltalkEnvironmentFrame(lastEnv);

			newEnvFrame.add(key, value.evaluate(lastEnv, receiver, c, globalInfo));
			lastEnv = newEnvFrame;
		}

		return this.expression.evaluate(lastEnv, receiver, c, globalInfo);
		// #endif
	}
}
