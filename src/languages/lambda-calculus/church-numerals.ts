// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/church-numerals.ts

import { ifDefinedThenElse } from 'thaw-common-utilities.ts';

import { ILCExpression } from './domain-object-model/interfaces/expression';

import { LCFunctionCall } from './domain-object-model/call';

import { LCLambdaExpression } from './domain-object-model/lambda-expression';

import { LCVariable } from './domain-object-model/variable';

import { isLCFunctionCall, isLCLambdaExpression } from './type-guards';

function integerToChurchNumeralHelper(
	n: number,
	varF: LCVariable,
	varX: LCVariable
): ILCExpression {
	if (n === 0) {
		return varX;
	}

	return new LCFunctionCall(varF, integerToChurchNumeralHelper(n - 1, varF, varX));
}

export function integerToChurchNumeral(
	n: number,
	options: { f?: string; x?: string } = {}
): ILCExpression {
	if (typeof n !== 'number' || Number.isNaN(n) || Math.round(n) !== n || n < 0) {
		throw new Error(`integerToChurchNumeral(${n}) : Bad parameter`);
	}

	const varF = new LCVariable(ifDefinedThenElse(options.f, 'f'));
	const varX = new LCVariable(ifDefinedThenElse(options.x, 'x'));

	return new LCLambdaExpression(
		varF,
		new LCLambdaExpression(varX, integerToChurchNumeralHelper(n, varF, varX))
	);
}

function churchNumeralToIntegerHelper(
	varF: LCVariable,
	varX: LCVariable,
	expr: ILCExpression,
	n: number
): number {
	if (varX.equals(expr)) {
		return n;
	} else if (!isLCFunctionCall(expr) || !varF.equals(expr.callee)) {
		return NaN;
	} else {
		return churchNumeralToIntegerHelper(varF, varX, expr.arg, n + 1);
	}
}

export function churchNumeralToInteger(expr: ILCExpression): number {
	// This function may return NaN.

	if (!isLCLambdaExpression(expr) || !isLCLambdaExpression(expr.body)) {
		return NaN;
	}

	return churchNumeralToIntegerHelper(expr.arg, expr.body.arg, expr.body.body, 0);
}

export function isChurchNumeral(expr: ILCExpression): boolean {
	return !Number.isNaN(churchNumeralToInteger(expr));
}
