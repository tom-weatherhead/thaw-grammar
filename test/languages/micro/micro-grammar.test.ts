// tom-weatherhead/thaw-grammar/test/languages/micro/micro-grammar.test.ts

'use strict';

import { createGrammar, LanguageSelector } from '../../..';

test('MicroGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.Micro);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
