// tom-weatherhead/thaw-grammar/test/languages/smalltalk/smalltalk-grammar.test.ts

'use strict';

import { createGrammar, LanguageSelector } from '../../..';

test('SmalltalkGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.Smalltalk);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
