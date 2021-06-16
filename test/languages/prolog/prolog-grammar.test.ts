// tom-weatherhead/thaw-grammar/test/languages/prolog/prolog-grammar.test.ts

'use strict';

import { createGrammar, LanguageSelector, PrologGlobalInfo } from '../../..';

test('PrologGrammar instance creation test', () => {
	// Arrange
	// const grammar = createGrammar(LanguageSelector.Prolog);
	const grammar = createGrammar(LanguageSelector.Prolog2);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});

test('PrologGlobalInfo instance creation test', () => {
	// Arrange
	// const grammar = createGrammar(LanguageSelector.Prolog);
	// const grammar = createGrammar(LanguageSelector.Prolog2);
	const globalInfo = new PrologGlobalInfo();

	// Act
	// Assert
	expect(globalInfo).toBeTruthy();
});
