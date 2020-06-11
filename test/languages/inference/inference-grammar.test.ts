// tom-weatherhead/thaw-grammar/test/languages/inference/inference-grammar.test.ts

'use strict';

import { createGrammar, LanguageSelector } from '../../../lib/main';

test('InferenceGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.Inference);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
