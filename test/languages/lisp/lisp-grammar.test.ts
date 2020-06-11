// tom-weatherhead/thaw-grammar/test/languages/lisp/lisp-grammar.test.ts

'use strict';

import { createGrammar, LanguageSelector } from '../../../lib/main';

test('LISPGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.LISP);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
