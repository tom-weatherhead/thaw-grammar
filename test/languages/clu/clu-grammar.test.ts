// tom-weatherhead/thaw-grammar/test/languages/clu/clu-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

// import { SyntaxException } from 'thaw-parser';

import { createFnRecognizer, createInfrastructure } from '../../create-infrastructure';

import { CLUEnvironmentFrame, CLUGlobalInfo, ICLUExpression, ICLUValue } from '../../..';

const ls = LanguageSelector.CLU;

function createFnEval(): (str: string) => ICLUValue {
	const { tokenizer, parser } = createInfrastructure(ls);
	const localEnvironment = new CLUEnvironmentFrame();

	const globalInfo = new CLUGlobalInfo();
	// Or: const globalInfo = new CLUGlobalInfo({ tokenizer, parser });

	// globalInfo.loadPresets(tokenizer, parser);

	return (str: string) => {
		const expr = parser.parse(tokenizer.tokenize(str)) as ICLUExpression;

		// The 'undefined' is the cluster.

		return expr.evaluate(localEnvironment, undefined, globalInfo);
	};
}

function evalStringsToValues(strs: string[], n = 1): ICLUValue[] {
	const f = createFnEval();

	return strs.map(f).slice(-n);
}

function evalStringsToStrings(strs: string[], n = 1): string[] {
	// return evalStringsToValues(strs, n).map((value) => value.toString());

	const f = createFnEval();

	return strs.map((str) => f(str).toString()).slice(-n);
}

function evalStringsToValue(strs: string[]): ICLUValue {
	const values = evalStringsToValues(strs, 1);

	if (values.length < 1) {
		throw new Error('evalToValue() : values.length is zero.');
	}

	return values[0];
}

function evalStringsToString(strs: string[]): string {
	return evalStringsToValue(strs).toString();
}

function evalStringToValue(str: string): ICLUValue {
	return evalStringsToValue([str]);
}

function evalStringToString(str: string): string {
	return evalStringToValue(str).toString();
}

test('CLUGrammar instance creation test', () => {
	// Arrange
	const { grammar } = createInfrastructure(ls);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});

test('CLUGrammar parser instance creation test', () => {
	// Arrange
	// Act
	const { parser } = createInfrastructure(ls);

	// Assert
	expect(parser).toBeTruthy();
});

test('CLUGrammar recognize test', () => {
	// Arrange
	const f = createFnRecognizer(ls);

	// From Kamin page 211.
	f(
		[
			'(cluster Point',
			'	(export new abscissa ordinate)',
			'	(rep x-coord y-coord)',
			'	(define new (x y) (Point x y))',
			'	(define abscissa (p) (x-coord p))',
			'	(define ordinate (p) (y-coord p))',
			')'
		].join(' ')
	);

	f('(Point$compare p1 p2)');

	f('(enclosed-area p1 p2)');

	// expect(() => f('')).toThrow(SyntaxException);
});

// private ICLUValue EvaluateToICLUValue(string input)
// {
// 	var expr = GetParseResult(input) as ICLUExpression;
//
// 	Assert.IsNotNull(expr);
//
// 	var cluExpr = expr.Evaluate(globalInfo.GlobalEnvironment, null, globalInfo);
//
// 	Assert.IsNotNull(cluExpr);
//
// 	return cluExpr;
// }
//
// private string Evaluate(string input)
// {
// 	return EvaluateToICLUValue(input).ToString();
// }

test('CLUGrammar point test', () => {
	const pointCluster = [
		// '; From Kamin, page 211 (not page 214)',
		// ';',
		'(cluster Point',
		'(export new abscissa ordinate reflect rotate compare quadrant)',
		'(rep x-coord y-coord)',
		'(define new (x y) (Point x y))',
		'(define abscissa (p) (x-coord p))',
		'(define ordinate (p) (y-coord p))',
		'(define reflect (p)',
		'(begin',
		'	(set-x-coord p (- 0 (x-coord p)))',
		'	(set-y-coord p (- 0 (y-coord p)))))',
		'(define rotate (p)',
		'(begin',
		'	(set temp (x-coord p))',
		'	(set-x-coord p (y-coord p))',
		'	(set-y-coord p (- 0 temp))))',
		'(define compare (p1 p2) (< (sqrdist p1) (sqrdist p2)))',
		'(define quadrant (p)',
		'(if (>= (x-coord p) 0)',
		'	(if (>= (y-coord p) 0) 1 2)',
		'	(if (< (y-coord p) 0) 3 4)))',
		// '; sqrdist is not exported',
		'(define sqrdist (p) (+ (sqr (x-coord p)) (sqr (y-coord p))))',
		')'
	].join(' ');

	const actualResults = evalStringsToStrings(
		[
			'(define not (x) (if x 0 1))',
			'(define >= (x y) (not (< x y)))',
			'(define sqr (x) (* x x))',
			'(define abs (x) (if (< x 0) (- 0 x) x))',
			pointCluster,
			'(set p1 (Point$new 3 4))',
			'p1', // -> x-coord = 3; y-coord = 4
			'(Point$rotate p1)',
			'p1', // -> x-coord = 4; y-coord = -3
			'(Point$abscissa p1)', // -> 4
			'(Point$ordinate p1)', // -> -3
			'(Point$reflect p1)',
			'(Point$abscissa p1)', // -> -4
			'(Point$ordinate p1)', // -> 3
			'(set p2 (Point$new 1 5))',
			'(Point$compare p1 p2)', // -> 1
			[
				'(define enclosed-area (p1 p2) (abs (*',
				'(- (Point$abscissa p1) (Point$abscissa p2))',
				'(- (Point$ordinate p1) (Point$ordinate p2)))))'
			].join(' '),
			'(enclosed-area p1 p2)' // -> 10
		],
		12
	);

	expect(actualResults.length).toBe(12);

	expect(actualResults[0]).toBe('x-coord = 3; y-coord = 4');
	expect(actualResults[2]).toBe('x-coord = 4; y-coord = -3');
	expect(actualResults[3]).toBe('4');
	expect(actualResults[4]).toBe('-3');
	expect(actualResults[6]).toBe('-4');
	expect(actualResults[7]).toBe('3');
	expect(actualResults[9]).toBe('1');
	expect(actualResults[11]).toBe('10');
});

test('CLUGrammar point test', () => {
	const listCluster = [
		// '; From Kamin, pages 216-217',
		// ';',
		'(cluster List',
		'(export nil null? cons car cdr rplaca rplacd)',
		'(rep type a d)',
		'(define nil() (List 0 0 0))',
		'(define null? (l) (= (type l) 0))',
		'(define cons (item l) (List 1 item l))',
		'(define car (l) (a l))', // ; apply selector a to l',
		'(define cdr (l) (d l))', // ; apply selector d to l',
		'(define rplaca (l a) (set-a l a))',
		'(define rplacd (l d) (set-d l d))',
		')'
	].join(' ');

	const actualResults = evalStringsToStrings(
		[
			'(define +1 (n) (+ n 1))',
			listCluster,
			'(set x (List$cons 1 (List$cons 2 (List$nil))))',
			'(set y x)',
			'(List$car x)',
			'(List$car y)',
			'(List$car (List$cdr x))',
			'(List$rplaca y 3)',
			'(List$car x)',
			'(List$car y)',
			'(define length (l) (if (List$null? l) 0 (+1 (length (List$cdr l)))))',
			'(length x)'
		],
		8
	);

	expect(actualResults.length).toBe(8);

	expect(actualResults[0]).toBe('1');
	expect(actualResults[1]).toBe('1');
	expect(actualResults[2]).toBe('2');
	expect(actualResults[4]).toBe('3');
	expect(actualResults[5]).toBe('3');
	expect(actualResults[6]).toBe('1');
	expect(actualResults[7]).toBe('2');

	// Assert.Throws<FunctionNotExportedException>(() => Evaluate("(List$type x)"));
	// Assert.Throws<FunctionNotExportedException>(() => Evaluate("(List$set-type x 0)"));
});

test('CLUGrammar cond test', () => {
	const actualResults = evalStringsToStrings(
		[
			'(define condtest (n) (cond ((= n 1) 101) ((= n 2) 102) ((= n 3) 103) (1 107)))',
			'(condtest 0)',
			'(condtest 1)',
			'(condtest 2)',
			'(condtest 3)',
			'(condtest 4)'
		],
		5
	);

	expect(actualResults.length).toBe(5);

	expect(actualResults[0]).toBe('107');
	expect(actualResults[1]).toBe('101');
	expect(actualResults[2]).toBe('102');
	expect(actualResults[3]).toBe('103');
	expect(actualResults[4]).toBe('107');
});

test('CLUGrammar let test', () => {
	expect(evalStringToString('(let ((n (+ 2 3))) n)')).toBe('5');
});

test('CLUGrammar let* test', () => {
	expect(evalStringToString('(let* ((x (+ 2 3)) (y (* x x))) y)')).toBe('25');
});

test('CLUGrammar AddBubbleDown test', () => {
	// 2013/12/04
	// This tests a bug fix to CLUEnvironmentFrame<T>.AddBubbleDown(), where Add() used to be called unconditionally at the end of the function.

	const actualResult = evalStringsToString([
		'(define test2 () foo)',
		[
			'(define test1 () (begin',
			'	(set foo 7)', // ; The buggy code was adding this variable to both the global and local frames.
			'	(set foo 13)', // ; The buggy code was modifying only the 'foo' in the local frame, since that's where AddBubbleDown() found 'foo' first.
			'	(test2)', // ; This returns the value of the 'foo' in the global frame.
			'))'
		].join(' '),
		'(test1)'
	]);

	expect(actualResult).toBe('13');
});

test('CLUGrammar TwoLocalEnvironments test', () => {
	// 2013/12/05
	const actualResult = evalStringsToString(['(define test (x) (let ((y 13)) x))', '(test 7)']);

	expect(actualResult).toBe('7');
});

test('CLUGrammar TwoLocalEnvironmentsSet test', () => {
	// 2013/12/06
	// To understand why this function contains two consecutive sets of the same variable, see the test AddBubbleDownTest().

	const actualResult = evalStringsToString([
		'(define test (x) (begin (let ((y 13)) (begin (set x 19) (set x 20))) x))',
		'(test 7)'
	]);

	expect(actualResult).toBe('20');
});
