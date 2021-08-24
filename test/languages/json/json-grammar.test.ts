// tom-weatherhead/thaw-grammar/test/languages/json/json-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { createGrammar } from '../../..';

test('JSONGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.JSON);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
