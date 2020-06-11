// tom-weatherhead/thaw-grammar/test/languages/minimal-language/minimal-language-grammar.test.ts

'use strict';

import { createGrammar, LanguageSelector } from '../../../lib/main';

test('MinimalLanguageGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.MinimalLanguage);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
