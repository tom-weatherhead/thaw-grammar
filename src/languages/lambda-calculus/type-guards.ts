// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/type-guards.ts

import {
	ILCExpression,
	ILCFunctionCall,
	ILCLambdaExpression,
	ILCVariable
} from './domain-object-model/interfaces/expression';

export const typenameLCFunctionCall = 'LCFunctionCall';
export const typenameLCLambdaExpression = 'LCLambdaExpression';
export const typenameLCVariable = 'LCVariable';

export function isLCFunctionCall(obj: unknown): obj is ILCFunctionCall {
	const otherLCFunctionCall = obj as ILCFunctionCall;

	return (
		typeof otherLCFunctionCall !== 'undefined' &&
		otherLCFunctionCall.typename === typenameLCFunctionCall
	);
}

export function isLCLambdaExpression(obj: unknown): obj is ILCLambdaExpression {
	const otherLCLambdaExpression = obj as ILCLambdaExpression;

	return (
		typeof otherLCLambdaExpression !== 'undefined' &&
		otherLCLambdaExpression.typename === typenameLCLambdaExpression
	);
}

export function isLCVariable(obj: unknown): obj is ILCVariable {
	const otherLCVariable = obj as ILCExpression;

	return (
		typeof otherLCVariable !== 'undefined' && otherLCVariable.typename === typenameLCVariable
	);
}
