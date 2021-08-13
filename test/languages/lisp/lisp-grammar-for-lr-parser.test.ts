// tom-weatherhead/thaw-grammar/test/languages/lisp/lisp-grammar-for-lr-parser.test.ts

'use strict';

import { LISPGrammarForLRParser } from '../../..';

test('LISPGrammarForLRParser instance creation test', () => {
	// Arrange
	const grammar = new LISPGrammarForLRParser();

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
