// tom-weatherhead/thaw-grammar/test/languages/lisp/lisp-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { createGrammar } from '../../..';

test('LISPGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.LISP);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
