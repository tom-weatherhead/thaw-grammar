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

// I.e. let var = e1 in e2

export function createStatementLetUsage(
	vv: LCVariable,
	e1: ILCExpression,
	e2: ILCExpression
): ILCExpression {
	return c(l(vv, e2), e1);
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

// export function createOperatorIncrementUsage(expression: ILCExpression, options: { f?: string; x?: string } = {}): ILCExpression {
// 	// λn.λf.λx.(f ((n f) x))
// 	const f = v(options.f, 'f');
// 	const x = v(options.x, 'x');
//
// 	return L(f, L(x, c(f, c(c(expression, f), x))));
// }
//
// export function createOperatorDecrementUsage(expression: ILCExpression): ILCExpression {
// 	// λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)
//
// 	return ;
// }
//
// export function createPredicateIsZeroUsage(expression: ILCExpression, options: { x?: string } = {}): ILCExpression {
// 	// λn.((n λx.FALSE) TRUE)
// 	const x = v(options.x, 'x');
//
// 	// return l(n, c(c(expression, l(n, createValueFalse())), createValueTrue()));
//
// 	return c(c(expression, l(x, createValueFalse())), createValueTrue());
// }

// export function createOperator(options: {} = {}): ILCExpression {
// 	;
// }

// **** Pairs (Tuples) ****

// From https://code.iamkate.com/lambda-calculus/tuples-and-lists/ :

// Tuples and lists in lambda calculus
//
// Tuples
//
// A tuple is a pair of functions. Using the lambda expressions below, a pair of a and b can be made using ‘pair a b’, and the first and second functions of p can be extracted using ‘first p’ and ‘second p’ respecively.
//
//     pair = λabf.fab -> λa.λb.λf.((f a) b) -> I.e. if
// 		- ((pair a) b) -> λf.((f a) b)
//     first = λp.p(λab.a) -> λp.(p (λa.λb.a)) -> I.e. λp.(p TRUE)
//     second = λp.p(λab.b) -> λp.(p (λa.λb.b)) -> I.e. λp.(p FALSE)

export function createPairUsage(
	first: ILCExpression,
	second: ILCExpression,
	options: { f?: string } = {}
): ILCExpression {
	const f = v(options.f, 'f');

	return l(f, c(c(f, first), second));
}

export function createFunctionCreatePair(
	options: { f?: string; a?: string; b?: string } = {}
): ILCExpression {
	const a = v(options.a, 'a');
	const b = v(options.b, 'b');

	return l(a, l(b, createPairUsage(a, b, options)));
}

export function createFunctionGetFirstOfPair(
	options: { x?: string; y?: string } = {}
): ILCExpression {
	return createValueTrue(options);
}

export function createFunctionGetSecondOfPair(
	options: { x?: string; y?: string } = {}
): ILCExpression {
	return createValueFalse(options);
}

// **** Lists ****

// A list is either empty, or consists of a head (any lambda expression) and a tail (another list). The most elegant way of representing a list is based on the representation of integer. The list containing a1, a2... ...an, is represented by λfx.fa1(fa2(...fan-1(fanx)...)). The lambda expression for the empty list, append function, head function and test for the empty list are:
//
// 	-? λf.λx.((f a1) (((f a2) (...((f an-1) ((f an) x)) ...))))
//
//     empty (i.e. the empty list?) = λf.λx.x -> I.e. the Church numeral zero
//     append = λa.λl.λf.λx.((f a) (((l f) x)))
//     head = λl.((l (λa.λb.a)) (any expression)) -> Gets the head of a list?
//     isempty = λl.((l (λa.λb.FALSE)) TRUE)
//
// ((append a) l) constructs a list with head a and tail l.

export function createEmptyList(options: { f?: string; x?: string } = {}): ILCExpression {
	// This is equivalent to the Church numeral zero.
	const f = v(options.f, 'f');
	const x = v(options.x, 'x');

	return l(f, l(x, x));
}

export function createFunctionAppend(
	options: { a?: string; l?: string; f?: string; x?: string } = {}
): ILCExpression {
	// append = λa.λl.λf.λx.((f a) ((l f) x))
	const a = v(options.a, 'a');
	const ll = v(options.l, 'l'); // Var named ll because of the func l() above.
	const f = v(options.f, 'f');
	const x = v(options.x, 'x');

	return l(a, l(ll, l(f, l(x, c(c(f, a), c(c(ll, f), x))))));
}

export function createFunctionGetHeadOfList(options: { l?: string } = {}): ILCExpression {
	// head = λl.((l (λa.λb.a)) (any expression))
	const ll = v(options.l, 'l');

	return l(ll, c(c(ll, createValueTrue()), ll));
}

export function createFunctionIsListEmpty(
	options: { a?: string; b?: string; l?: string } = {}
): ILCExpression {
	// isempty = λl.((l (λa.λb.FALSE)) TRUE)
	const a = v(options.a, 'a');
	const b = v(options.b, 'b');
	const ll = v(options.l, 'l');

	return l(ll, c(c(ll, l(a, l(b, createValueFalse()))), createValueTrue()));
}

// The tail function is more complicated, and makes use of the tuples defined above. The principle is to start off with a pair (empty, empty), and at each stage turn the pair (x, y) into the pair (y, ((append a) y)), where a is the current list element, and then take the first element of the pair. The lambda expression is:
//
//     tail = λl.first(l(λab.pair(second b)(append a (second b)))(pair empty empty))
// 		-> λl.(first (l (λa.λb.(pair (second b)) ((append a) (second b))) ((pair empty) empty)))

export function createFunctionGetTailOfList(
	options: { l?: string; a?: string; b?: string } = {}
): ILCExpression {
	// tail = λl.first(
	//   l(λab.pair(second b)(append a (second b)))
	//   (pair empty empty)
	// )
	// -> λl.(first (l
	//   (
	//     λa.λb.(pair (second b))
	//     ((append a) (second b))
	//   )
	//   ((pair empty) empty)))
	const ll = v(options.l, 'l');

	const cp = createFunctionCreatePair();
	const f = createFunctionGetFirstOfPair();
	const s = createFunctionGetSecondOfPair();
	const e = createEmptyList();
	const ap = createFunctionAppend();

	const a = v(options.a, 'a');
	const b = v(options.b, 'b');

	return l(
		ll,
		c(
			f,
			c(
				ll, // Calls No. 1 and 2
				c(
					// BEGIN Call No. 3
					// BEGIN Callee No. 3
					l(
						a,
						l(
							b,
							c(
								// Create a pair : Takes 2 args, so make 2 calls
								c(cp, c(s, b)), // The pair's first element
								c(c(ap, a), c(s, b))
							)
						) // The pair's second element
					), // END of creation of pair
					// END Callee No. 3
					c(c(cp, e), e) // Argument for call No. 3
				) // END Call No. 3
			)
		)
	);
}

// - In LISP terms:
// 	- head = car
// 	- tail = cdr
// 	- cons? = append? -> It looks like append adds a onto the head of l, not the tail.
// 		cons = λa.λl.λf.λx.?((f a) (l f) x)

export function createCombinator(name: string): ILCExpression {
	const x = new LCVariable('x');

	switch (name) {
		case 'I':
			return l(x, x);

		default:
			throw new Error(
				`createCombinator() : Combinator '${name}' is either unimplemented or non-existent.`
			);
	}
}
