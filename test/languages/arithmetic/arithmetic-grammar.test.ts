// tom-weatherhead/thaw-grammar/test/languages/arithmetic/arithmetic-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { createGrammar } from '../../..';

import { createFnRecognizer /*, createInfrastructure */ } from '../../create-infrastructure';

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
	// f('3 - 2');
	f('1 + 2 + 3');

	// expect(() => f('(* 7 13')).toThrow(SyntaxException);
});
