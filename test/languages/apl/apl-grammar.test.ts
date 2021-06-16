// tom-weatherhead/thaw-grammar/test/languages/apl/apl-grammar.test.ts

'use strict';

import { createGrammar, LanguageSelector } from '../../..';

test('APLGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.APL);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
