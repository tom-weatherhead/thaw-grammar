// tom-weatherhead/thaw-grammar/test/languages/scheme/scheme-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { createGrammar } from '../../..';

test('SchemeGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.Scheme);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
