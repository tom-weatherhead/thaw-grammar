// tom-weatherhead/thaw-grammar/test/languages/sasl/sasl-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { createGrammar } from '../../..';

test('SASLGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.SASL);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
