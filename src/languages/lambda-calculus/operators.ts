// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/operators.ts

import { ifDefinedThenElse } from 'thaw-common-utilities.ts';

import { ILCExpression } from './domain-object-model/interfaces/expression';

import { LCFunctionCall } from './domain-object-model/call';

import { LCLambdaExpression } from './domain-object-model/lambda-expression';

import { LCVariable } from './domain-object-model/variable';

// import { isChurchNumeral } from './church-numerals';

function v(s1: string | undefined, s2: string): LCVariable {
	return new LCVariable(ifDefinedThenElse(s1, s2));
}

function l(v: LCVariable, e: ILCExpression): LCLambdaExpression {
	return new LCLambdaExpression(v, e);
}

function c(e1: ILCExpression, e2: ILCExpression): LCFunctionCall {
	return new LCFunctionCall(e1, e2);
}

export function createValueTrue(options: { x?: string; y?: string } = {}): ILCExpression {
	const x = v(options.x, 'x');
	const y = v(options.y, 'y');

	return l(x, l(y, x));
}

export function createValueFalse(options: { x?: string; y?: string } = {}): ILCExpression {
	const x = v(options.x, 'x');
	const y = v(options.y, 'y');

	return l(x, l(y, y));
}

export function createOperatorIfUsage(
	condition: ILCExpression,
	thenPart: ILCExpression,
	elsePart: ILCExpression
): ILCExpression {
	return c(c(condition, thenPart), elsePart);
}

export function createOperatorIf(
	options: { b?: string; x?: string; y?: string } = {}
): ILCExpression {
	// if : λb.λx.λy.((b x) y)

	const b = v(options.b, 'b');
	const x = v(options.x, 'x');
	const y = v(options.y, 'y');

	// return l(b, l(x, l(y, c(c(b, x), y))));

	return l(b, l(x, l(y, createOperatorIfUsage(b, x, y))));
}

export function createOperatorAndUsage(expr1: ILCExpression, expr2: ILCExpression): ILCExpression {
	// && (and) : λp.λq.((p q) FALSE)
	return c(c(expr1, expr2), createValueFalse());
}

export function createOperatorAnd(options: { p?: string; q?: string } = {}): ILCExpression {
	// && (and) : λp.λq.((p q) FALSE)
	const p = v(options.p, 'p');
	const q = v(options.q, 'q');

	return l(p, l(q, createOperatorAndUsage(p, q)));
}

export function createOperatorOrUsage(expr1: ILCExpression, expr2: ILCExpression): ILCExpression {
	// || (or) : λp.λq.(((IF p) TRUE) q)
	return createOperatorIfUsage(expr1, createValueTrue(), expr2);
}

export function createOperatorOr(options: { p?: string; q?: string } = {}): ILCExpression {
	// || (or) : λp.λq.(((IF p) TRUE) q)
	const p = v(options.p, 'p');
	const q = v(options.q, 'q');

	return l(p, l(q, createOperatorOrUsage(p, q)));
}

export function createOperatorIsZeroUsage(
	expr: ILCExpression,
	options: { x?: string } = {}
): ILCExpression {
	// (z? or 0?) (isZero) : λn.((n λx.FALSE) TRUE)
	const x = v(options.x, 'x');

	return c(c(expr, l(x, createValueFalse())), createValueTrue());
}

export function createOperatorIsZero(options: { n?: string; x?: string } = {}): ILCExpression {
	// (z? or 0?) (isZero) : λn.((n λx.FALSE) TRUE)
	const n = v(options.n, 'n');

	return l(n, createOperatorIsZeroUsage(n));
}

export function createOperatorIncrementUsage(
	expr: ILCExpression,
	options: { f?: string; x?: string } = {}
): ILCExpression {
	// ++ (successor) : λn.λf.λx.(f ((n f) x))
	const f = v(options.f, 'f');
	const x = v(options.x, 'x');

	return l(f, l(x, c(f, c(c(expr, f), x))));
}

export function createOperatorIncrement(
	options: { n?: string; f?: string; x?: string } = {}
): ILCExpression {
	// ++ (successor) : λn.λf.λx.(f ((n f) x))
	const n = v(options.n, 'n');

	return l(n, createOperatorIncrementUsage(n, options));
}

export function createOperatorDecrementUsage(
	expr: ILCExpression,
	options: { f?: string; x?: string } = {}
): ILCExpression {
	// -- (predecessor) : λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)
	const f = v(options.f, 'f');
	const x = v(options.x, 'x');
	const g = new LCVariable('g');
	const h = new LCVariable('h');
	const u = new LCVariable('u');

	return l(f, l(x, c(c(c(expr, l(g, l(h, c(h, c(g, f))))), l(u, x)), l(u, u))));
}

export function createOperatorDecrement(
	options: { n?: string; f?: string; x?: string } = {}
): ILCExpression {
	// -- (predecessor) : λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)
	const n = v(options.n, 'n');

	return l(n, createOperatorDecrementUsage(n, options));
}

export function createOperatorAddUsage(
	expr1: ILCExpression,
	expr2: ILCExpression,
	options: { f?: string; x?: string } = {}
): ILCExpression {
	// + : λm.λn.λf.λx.((n f) ((m f) x))
	const f = v(options.f, 'f');
	const x = v(options.x, 'x');

	return l(f, l(x, c(c(expr2, f), c(c(expr1, f), x))));
}

export function createOperatorAdd(
	options: { m?: string; n?: string; f?: string; x?: string } = {}
): ILCExpression {
	// + : λm.λn.λf.λx.((n f) ((m f) x))
	const m = v(options.m, 'm');
	const n = v(options.n, 'n');

	return l(m, l(n, createOperatorAddUsage(m, n, options)));
}

export function createOperatorMultiplyUsage(
	expr1: ILCExpression,
	expr2: ILCExpression,
	options: { f?: string } = {}
): ILCExpression {
	// * : λm.λn.λf.(m (n f))
	const f = v(options.f, 'f');

	return l(f, c(expr1, c(expr2, f)));
}

export function createOperatorMultiply(
	options: { m?: string; n?: string; f?: string } = {}
): ILCExpression {
	// * : λm.λn.λf.(m (n f))
	const m = v(options.m, 'm');
	const n = v(options.n, 'n');

	return l(m, l(n, createOperatorMultiplyUsage(m, n, options)));
}

// export function createOperator(options: {} = {}): ILCExpression {
// 	;
// }
