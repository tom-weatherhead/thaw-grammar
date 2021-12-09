// tom-weatherhead/thaw-grammar/test/languages/arithmetic/arithmetic-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { ArithmeticGlobalInfo, createGrammar, IExpression } from '../../..';

import { createFnRecognizer, createInfrastructure } from '../../create-infrastructure';

const ls = LanguageSelector.Arithmetic;

test('ArithmeticGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.Arithmetic);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});

test('ArithmeticGrammar recognize test', () => {
	// Arrange
	const f = createFnRecognizer(ls);

	f('1');
	f('1 + 1');
	f('3 - 2');
	f('1 + 2 + 3');

	// expect(() => f('(* 7 13')).toThrow(SyntaxException);
});

function arithmeticTest(
	data: Array<[input: string, expectedResult: number]> // ,
	// options: {
	// 	presets?: string[];
	// } = {}
): void {
	// Arrange
	const { tokenizer, parser } = createInfrastructure(ls);
	const globalInfo = new ArithmeticGlobalInfo({ tokenizer, parser });

	// globalInfo.loadPresets();

	// if (typeof options.presets !== 'undefined') {
	// 	for (const preset of options.presets) {
	// 		schemeGlobalInfo.loadPreset(preset);
	// 	}
	// }

	for (const [input, expectedResult] of data) {
		// Act
		const parseResult = parser.parse(tokenizer.tokenize(input));
		const expr = parseResult as IExpression<number>;
		const actualResult = expr.evaluate(globalInfo, globalInfo.globalEnvironment);

		// console.log(`input: ${input}\nactualResult:\n${actualResult}\n\n`);

		// Assert
		// if (typeof expectedResult === 'string') {
		expect(actualResult).toBe(expectedResult);
		// } else {
		// 	for (const str of expectedResult) {
		// 		expect(actualResult.includes(str)).toBe(true);
		// 	}
		// }
	}
}

test('Arithmetic addition test 1', () => {
	arithmeticTest([['2 + 3', 5]]);
});

test('Arithmetic subtraction test 1', () => {
	arithmeticTest([['9 - 2', 7]]);
});

test('Arithmetic subtraction test 2', () => {
	arithmeticTest([['9 - 2 - 3', 4]]);
});

test('Arithmetic addition and subtraction test 1', () => {
	arithmeticTest([['9 - (2 - 3)', 10]]);
});

test('Arithmetic multiplication test 1', () => {
	arithmeticTest([['2 * 3', 6]]);
});

test('Arithmetic mixed precedence test 1', () => {
	arithmeticTest([['1 + 3 * 4', 13]]);
});

test('Arithmetic mixed precedence test 2', () => {
	arithmeticTest([['1 * 2 + 3 * 4 + 5 * 6', 44]]);
});
