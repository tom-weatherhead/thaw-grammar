// tom-weatherhead/thaw-grammar/test/languages/chapter1/chapter1-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { createGrammar } from '../../..';

test('Chapter1Grammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.Chapter1);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
