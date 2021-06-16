// tom-weatherhead/thaw-grammar/test/languages/prolog/prolog-grammar.test.ts

'use strict';

import { createGrammar, LanguageSelector } from '../../..';

test('PrologGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.Prolog);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
