// tom-weatherhead/thaw-grammar/test/languages/lambda-calculus/lambda-calculus-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { createGrammar } from '../../..';

test('LambdaCalculusGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.LambdaCalculus);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});

// test('LambdaCalculusGrammar recognize test', () => {
// 	// Arrange
// 	const grammar = createGrammar(LanguageSelector.LambdaCalculus);
//
// 	// Act
// 	// Assert
// 	expect(grammar).toBeTruthy();
// });
