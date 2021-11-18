// tom-weatherhead/thaw-grammar/test/languages/apl/apl-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

// import { SyntaxException } from 'thaw-parser';

import {
	APLGlobalInfo,
	createAPLNullValue,
	EnvironmentFrame,
	IAPLExpression,
	IAPLValue
} from '../../..';

import { createFnRecognizer, createInfrastructure } from '../../create-infrastructure';

// Define values for unit testing here.

const testvector1 = "'(1 1 2 3)";
const testvector2 = "'(5 8 13 21)";
const logicalvector3 = "'(1 0 1)";
const logicalvector4 = "'(0 1 1 0)";
// Evaluate("(set floatvector5 '(1.0 1.25 1.5 1.75))");
// Evaluate("(set floatvector6 '(2.0 3.5 5.0 7.5))");
// Evaluate("(set testmatrix1 (restruct '(3 4) '(1 2 3 4 5 6 7 8 9 10 11 12)))");
const testmatrix2 = "(restruct '(3 4) '(2 3 5 7 11 13 17 19 23 29 31 37))";
// Evaluate("(set logicalmatrix3 (restruct '(3 4) '(1 1 1 1 0 0 0 0 1 1 1 1)))");
// Evaluate("(set logicalmatrix4 (restruct '(3 4) '(0 1 0 1 0 1 0 1 0 1 0 1)))");

const ls = LanguageSelector.APL;

function createFnEval(): (str: string) => IAPLValue {
	const { tokenizer, parser } = createInfrastructure(ls);
	const localEnvironment = new EnvironmentFrame<IAPLValue>();

	const globalInfo = new APLGlobalInfo();
	// Or: const globalInfo = new APLGlobalInfo({ tokenizer, parser });

	// globalInfo.loadPresets(tokenizer, parser);

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

function evalStringsToString(strs: string[]): string {
	return evalStringsToValue(strs).toString();
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

test('APLGrammar subtraction test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString('(- 2 3)')).toBe('-1');
});

test('APLGrammar multiplication test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString('(* 2 3)')).toBe('6');
});

test('APLGrammar truncating integer division test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString('(/ 6 3)')).toBe('2');
	expect(evalStringToString('(/ 7 3)')).toBe('2');
	expect(evalStringToString('(/ 8 3)')).toBe('2');
	expect(evalStringToString('(/ 9 3)')).toBe('3');
});

// case 'max':
// case 'or':
// case 'and':

test('APLGrammar max test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString('(max 2 3)')).toBe('3');
});

test('APLGrammar or test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString('(or 0 0)')).toBe('0');
	expect(evalStringToString('(or 0 1)')).toBe('1');
	expect(evalStringToString('(or 1 0)')).toBe('1');
	expect(evalStringToString('(or 1 1)')).toBe('1');
});

test('APLGrammar and test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString('(and 0 0)')).toBe('0');
	expect(evalStringToString('(and 0 1)')).toBe('0');
	expect(evalStringToString('(and 1 0)')).toBe('0');
	expect(evalStringToString('(and 1 1)')).toBe('1');
});

test('APLGrammar restruct test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString("(restruct '(2 2) '(1 2 3 4))")).toBe('1 2\n3 4');
	expect(evalStringToString("(restruct '(2 2) '(1 2 3 4))")).toBe(['1 2', '3 4'].join('\n'));

	// TODO:

	// 	Assert.AreEqual("5", Evaluate("(restruct '() testvector2)"));
	// 	Assert.AreEqual("5 8 13", Evaluate("(restruct 3 testvector2)"));
	// 	Assert.AreEqual("5 8 13", Evaluate("(restruct '(3) testvector2)"));
	// 	Assert.AreEqual("5 8\r\n13 21", Evaluate("(restruct '(2 2) testvector2)"));
	//
	// 	// A three-dimensional matrix:
	// 	Assert.AreEqual(@"Slice (0) :
	// 1 2 3 4
	// 5 6 7 8
	// 9 10 11 12
	//
	// Slice (1) :
	// 13 14 15 16
	// 17 18 19 20
	// 21 22 23 24", Evaluate("(restruct '(2 3 4) (indx 24))"));
	//
	// 	// A four-dimensional matrix:
	// 	Assert.AreEqual(@"Slice (0, 0) :
	// 1 2 3 4
	// 5 6 7 8
	// 9 10 11 12
	//
	// Slice (0, 1) :
	// 13 14 15 16
	// 17 18 19 20
	// 21 22 23 24
	//
	// Slice (1, 0) :
	// 25 26 27 28
	// 29 30 31 32
	// 33 34 35 36
	//
	// Slice (1, 1) :
	// 37 38 39 40
	// 41 42 43 44
	// 45 46 47 48", Evaluate("(restruct '(2 2 3 4) (indx 48))"));
	//
	// 	// Another four-dimensional matrix:
	// 	Assert.AreEqual(@"Slice (0, 0) :
	// 1 2 3 4
	// 5 6 7 8
	//
	// Slice (0, 1) :
	// 9 10 11 12
	// 13 14 15 16
	//
	// Slice (0, 2) :
	// 17 18 19 20
	// 21 22 23 24
	//
	// Slice (1, 0) :
	// 25 26 27 28
	// 29 30 31 32
	//
	// Slice (1, 1) :
	// 33 34 35 36
	// 37 38 39 40
	//
	// Slice (1, 2) :
	// 41 42 43 44
	// 45 46 47 48", Evaluate("(restruct '(2 3 2 4) (indx 48))"));
});

test('APLGrammar shape test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString("(shape (restruct '(2 2) '(1 2 3 4)))")).toBe('2 2');
});

test('APLGrammar ravel test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString("(ravel (restruct '(2 2) '(1 2 3 4)))")).toBe('1 2 3 4');
});

test('APLGrammar indx test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString('(indx 0)')).toBe('');
	expect(evalStringToString('(indx 7)')).toBe('1 2 3 4 5 6 7');
	expect(evalStringToString("(indx '(6 12 18))")).toBe('1 2 3 4 5 6');
	expect(evalStringToString("(indx (restruct '(2 2) '(5 7 2 3)))")).toBe('1 2 3 4 5');

	// Assert.Throws<Exception>(() => Evaluate("(indx 7.5)"));
	// Assert.Throws<Exception>(() => Evaluate("(indx '(6.25 12.5 18.75))"));
	// Assert.Throws<Exception>(() => Evaluate("(indx (restruct '(2 2) '(5.0 7.0 2.0 3.0)))"));
});

test('APLGrammar trans test', () => {
	// Matrix transposition
	// Arrange
	// Act
	// Assert

	// expect(evalStringToString("(trans (restruct '(2 2) '(1 2 3 4)))")).toBe('1 2 3 4');

	// 	// A scalar (no-op):
	// 	Assert.AreEqual("7", Evaluate("(trans 7)"));
	expect(evalStringToString('(trans 7)')).toBe('7');

	// 	// A vector (no-op):
	// 	Assert.AreEqual("2 3 5 7", Evaluate("(trans '(2 3 5 7))"));
	//
	// 	// A two-dimensional matrix:
	// 	Assert.AreEqual("2 11 23\r\n3 13 29\r\n5 17 31\r\n7 19 37", Evaluate("(trans testmatrix2)"));
	//
	// 	// A three-dimensional matrix:
	// 	var m3string = Evaluate("(set m3 (restruct '(2 3 4) (indx 24)))");
	//
	// 	Assert.AreNotEqual(m3string, Evaluate("(set m3a (trans m3))"));
	// 	Assert.AreNotEqual(m3string, Evaluate("(set m3b (trans m3a))"));
	// 	Assert.AreEqual(m3string, Evaluate("(trans m3b)"));
	//
	// 	// A four-dimensional matrix:
	// 	var m4string = Evaluate("(set m4 (restruct '(2 3 5 7) (indx 210)))");
	//
	// 	Assert.AreNotEqual(m4string, Evaluate("(set m4a (trans m4))"));
	// 	Assert.AreNotEqual(m4string, Evaluate("(set m4b (trans m4a))"));
	// 	Assert.AreNotEqual(m4string, Evaluate("(set m4c (trans m4b))"));
	// 	Assert.AreEqual(m4string, Evaluate("(trans m4c)"));
});

test('APLGrammar compress test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString(`(compress '(0 0 0 0) ${testvector2})`)).toBe('');
	expect(evalStringToString(`(compress ${logicalvector4} ${testvector2})`)).toBe('8 13');
	expect(evalStringToString(`(compress '(0 0 0) ${testmatrix2})`)).toBe('');
	expect(evalStringToString(`(compress ${logicalvector3} ${testmatrix2})`)).toBe(
		'2 3 5 7\n23 29 31 37'
	);
});

test('APLGrammar cat test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString('(cat 7 13)')).toBe('7 13');
	expect(evalStringToString(`(cat ${testvector1} ${testvector2})`)).toBe('1 1 2 3 5 8 13 21');
});

test('APLGrammar [] (subscripting) test', () => {
	// Arrange
	// Act
	// Assert

	// Assert.AreEqual("13", Evaluate("([] testvector2 3)"));
	expect(evalStringToString(`([] ${testvector2} 3)`)).toBe('13');
	// Assert.AreEqual("8 5 21", Evaluate("([] testvector2 '(2 1 4))"));
	expect(evalStringToString(`([] ${testvector2} '(2 1 4))`)).toBe('8 5 21');
	// Assert.AreEqual("11 13 17 19", Evaluate("([] testmatrix2 2)"));
	expect(evalStringToString(`([] ${testmatrix2} 2)`)).toBe('11 13 17 19');
	// Assert.AreEqual("23 29 31 37\r\n2 3 5 7\r\n2 3 5 7", Evaluate("([] testmatrix2 '(3 1 1))"));
	expect(evalStringToString(`([] ${testmatrix2} '(3 1 1))`)).toBe(
		'23 29 31 37\n2 3 5 7\n2 3 5 7'
	);
});

test('APLGrammar [;] (double subscripting) test', () => {
	// Arrange
	// Act
	// Assert

	// 	Assert.AreEqual("7 9\r\n17 19", Evaluate("([;] (restruct '(5 5) (indx 25)) '(2 4) '(2 4))"));
	expect(evalStringToString("([;] (restruct '(5 5) (indx 25)) '(2 4) '(2 4))")).toBe(
		'7 9\n17 19'
	);

	// 	Assert.AreEqual("1 4 5\r\n6 9 10", Evaluate("([;] (restruct '(4 5) (indx 20)) '(1 2) '(1 4 5))"));
	expect(evalStringToString("([;]  (restruct '(4 5) (indx 20)) '(1 2) '(1 4 5))")).toBe(
		'1 4 5\n6 9 10'
	);
});

test('APLGrammar := (vector assignment) test', () => {
	// Arrange
	// Act
	// Assert

	expect(
		evalStringsToString(["(set vector1 '(10 20 30 40 50))", '(:= vector1 4 13)', 'vector1'])
	).toBe('10 20 30 13 50');

	expect(
		evalStringsToString([
			"(set vector2 '(10 20 30 40 50))",
			"(:= vector2 '(3 5 1) '(7 9 11))",
			'vector2'
		])
	).toBe('11 20 7 40 9');
});

test('APLGrammar addition reduction test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString("(+/ '(1 2 3 4))")).toBe('10');
	expect(evalStringToString("(+/ (restruct '(3 3) '(1 2 3 4 5 6 7 8 9)))")).toBe('6 15 24');
});

test('APLGrammar subtraction reduction test', () => {
	// Arrange
	// Act
	// Assert

	// The reduction is performed from right to left; e.g. 1 - (2 - (3 - 4)).
	expect(evalStringToString("(-/ '(1 2 3 4))")).toBe('-2');
	expect(evalStringToString("(-/ (restruct '(3 3) '(1 2 3 4 5 6 7 8 9)))")).toBe('2 5 8');
});

test('APLGrammar multiplication reduction test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString("(*/ '(1 2 3 4))")).toBe('24');
	expect(evalStringToString("(*/ (restruct '(3 3) '(1 2 3 4 5 6 7 8 9)))")).toBe('6 120 504');
});

test('APLGrammar division reduction test', () => {
	// Arrange
	// Act
	// Assert

	// The reduction is performed from right to left; e.g. 4 / (4 / (4 / 2)).
	expect(evalStringToString("(// '(5040 7))")).toBe('720');
	expect(evalStringToString("(// '(4 4 4 4 4 4 4 2))")).toBe('2');
	expect(evalStringToString("(// (restruct '(3 3) '(24 12 3 120 30 6 144 48 12)))")).toBe(
		'6 24 36'
	);
});

test('APLGrammar max reduction test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString("(max/ '(1 2 3 4))")).toBe('4');
	expect(evalStringToString("(max/ (restruct '(3 3) '(1 2 3 6 5 4 7 9 8)))")).toBe('3 6 9');
});

test('APLGrammar and reduction test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString("(and/ (restruct '(4 2) '(0 0 0 1 1 0 1 1)))")).toBe('0 0 0 1');
});

test('APLGrammar or reduction test', () => {
	// Arrange
	// Act
	// Assert

	expect(evalStringToString("(or/ (restruct '(4 2) '(0 0 0 1 1 0 1 1)))")).toBe('0 1 1 1');
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

test('APLGrammar cond test', () => {
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

test('APLGrammar let test', () => {
	expect(evalStringToString('(let ((n (+ 2 3))) n)')).toBe('5');
});

test('APLGrammar let* test', () => {
	expect(evalStringToString('(let* ((x (+ 2 3)) (y (* x x))) y)')).toBe('25');
});
