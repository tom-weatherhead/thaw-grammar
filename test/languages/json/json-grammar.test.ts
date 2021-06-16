// tom-weatherhead/thaw-grammar/test/languages/json/json-grammar.test.ts

'use strict';

import { createGrammar, LanguageSelector } from '../../..';

test('JSONGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.JSON);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
