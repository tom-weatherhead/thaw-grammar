// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/utilities-with-dom.ts

import { ILCExpression } from './domain-object-model/interfaces/expression';

import { LCFunctionCall } from './domain-object-model/call';

// import { LCLambdaExpression } from './domain-object-model/lambda-expression';

// import { LCVariable } from './domain-object-model/variable';

// E.g. (((f arg1) arg2) arg3)
export function callMultiArgumentFunction(
	f: ILCExpression,
	...args: ILCExpression[]
): ILCExpression {
	if (args.length === 0) {
		throw new Error('callMultiArgumentFunction() : Zero arguments');
	} else if (args.length === 1) {
		return new LCFunctionCall(f, args[0]);
	} else {
		return new LCFunctionCall(
			callMultiArgumentFunction(f, ...args.slice(0, args.length - 1)),
			args[args.length - 1]
		);
	}
}
