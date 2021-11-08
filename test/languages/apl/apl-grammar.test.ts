// tom-weatherhead/thaw-grammar/test/languages/apl/apl-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

// import { createParser /*, SyntaxException */ } from 'thaw-parser';

import {
	APLGlobalInfo,
	createAPLNullValue,
	EnvironmentFrame,
	IAPLExpression,
	IAPLValue
} from '../../..';

import {
	// createFnParser,
	createFnRecognizer,
	createInfrastructure
} from '../../create-infrastructure';

const ls = LanguageSelector.APL;

function createFnEval(): (str: string) => IAPLValue {
	const { tokenizer, parser } = createInfrastructure(ls);
	const localEnvironment = new EnvironmentFrame<IAPLValue>();
	const globalInfo = new APLGlobalInfo();

	// globalInfo.loadPresets(tokenizer, parser);

	// return (str: string) =>
	// 	globalInfo.evaluate(parser.parse(tokenizer.tokenize(str)) as IAPLExpression);

	return (str: string) => {
		const expr = parser.parse(tokenizer.tokenize(str)) as IAPLExpression;

		return expr.evaluate(localEnvironment, globalInfo);
	};
}

function evalStringsToValues(strs: string[], n = 1): IAPLValue[] {
	const f = createFnEval();

	return strs.map(f).slice(-n);
}

function evalStringsToStrings(strs: string[], n = 1): string[] {
	return evalStringsToValues(strs, n).map((value) => value.toString());
}

function evalStringsToValue(strs: string[]): IAPLValue {
	const values = evalStringsToValues(strs, 1);

	if (values.length < 1) {
		throw new Error('evalToValue() : values.length is zero.');
	}

	return values[0];
}

function evalStringToValue(str: string): IAPLValue {
	return evalStringsToValue([str]);
}

function evalStringToString(str: string): string {
	return evalStringToValue(str).toString();
}

// function evaluateStringToInteger(str: string): number | undefined {
// 	return evalStringsToValue([str]).toInteger();
// }
//
// function evaluateStringsToInteger(strs: string[]): number | undefined {
// 	return evalStringsToValue(strs).toInteger();
// }
//
// function evaluateStringsToIntegers(strs: string[], n = 1): Array<number | undefined> {
// 	return evalStringsToValues(strs, n).map((value) => value.toInteger());
// }

test('APLGrammar instance creation test', () => {
	// Arrange
	const { grammar } = createInfrastructure(ls);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});

test('APLGrammar parser instance creation test', () => {
	// Arrange
	// Act
	const { parser } = createInfrastructure(ls);

	// Assert
	expect(parser).toBeTruthy();
});

test('APLGrammar recognize test', () => {
	// Arrange
	const f = createFnRecognizer(ls);

	f('(define fac (n) (*/ (indx n)))'); // Page 70
	f('(define avg (v) (/ (+/ v) (shape v)))');
	f('(define neg (v) (- 0 v))');
	f('(define min (v1 v2) (neg (max (neg v1) (neg v2))))');
	f('(define min/ (v) (neg (max/ (neg v))))');
	f('(define not (x) (- 1 x))'); // Page 71
	f('(define <> (x y) (not (= x y)))');
	f('(define signum (x) (+ (* (< x 0) -1) (> x 0)))'); // Page 72

	// expect(() => f('')).toThrow(SyntaxException);
});

test('APLGrammar null value test', () => {
	// Arrange
	// Act
	// Assert

	expect(createAPLNullValue().isNull).toBe(true);

	expect(evalStringToValue('7').isNull).toBe(false);
	expect(evalStringToValue("'(2 3 5 7)").isNull).toBe(false);
	expect(evalStringToValue("(restruct '(2 2) '(1 2 3 4))").isNull).toBe(false);
});

test('APLGrammar addition test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString('(+ 2 3)')).toBe('5');
	expect(evalStringToString("(+ 2 '(3 6))")).toBe('5 8');
	expect(evalStringToString("(+ '(1 2) 10)")).toBe('11 12');
	expect(evalStringToString("(+ '(1 2 7) '(3 4 9))")).toBe('4 6 16');

	// Non-scalars with different shapes cannot be added:
	expect(() => evalStringToString("(+ '(1 2) '(3 4 9))")).toThrow(Error);
	expect(() => evalStringToString("(+ '(1 2 7) '(3 4))")).toThrow(Error);
});

test('APLGrammar restruct test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString("(restruct '(2 2) '(1 2 3 4))")).toBe('1 2\n3 4');
	expect(evalStringToString("(restruct '(2 2) '(1 2 3 4))")).toBe(['1 2', '3 4'].join('\n'));
});

test('APLGrammar define (user-defined functions) test', () => {
	// Arrange
	// Act

	const actualResults = evalStringsToStrings(
		['(define neg (v) (- 0 v))', '(neg 7)', '(neg -3)', '(neg 0)'],
		3
	);

	// Assert
	expect(actualResults.length).toBe(3);
	expect(actualResults[0]).toBe('-7');
	expect(actualResults[1]).toBe('3');
	expect(actualResults[2]).toBe('0');
});
