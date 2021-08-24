// tom-weatherhead/thaw-grammar/test/languages/clu/clu-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { createGrammar } from '../../..';

test('CluGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.CLU);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
