//

'use strict';

import { LanguageSelector, LexicalAnalyzerSelector, ParserSelector } from 'thaw-interpreter-types';

import { createTokenizer } from 'thaw-lexical-analyzer';

import { areIsomorphic, createGrammar, ILCExpression, ILCVariable } from '../../..';

import { createParser, SyntaxException } from 'thaw-parser';

const ls = LanguageSelector.LambdaCalculusIntegerExtension;

test('LambdaCalculusIntegerExtensionGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.LambdaCalculusIntegerExtension);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});

test('LambdaCalculusIntegerExtensionGrammar recognize test', () => {
	// Arrange
	// const grammar = createGrammar(LanguageSelector.LambdaCalculusIntegerExtension);

	// Act
	// Assert
	// Arrange
	const grammar = createGrammar(ls);
	const tokenizer = createTokenizer(LexicalAnalyzerSelector.MidnightHack, ls);
	const parser = createParser(ParserSelector.LL1, grammar);

	const f = (str: string): void => parser.recognize(tokenizer.tokenize(str));

	// For a list of combinators, see https://github.com/loophp/combinator ,
	// all (?) of which can be implemented in terms of just S and K.

	f('x');
	f('λx.x'); // Combinator I (identity) === ((S K) K)
	f('λx.λy.x'); // Combinator K
	f('(x y)');
	f('(λx.x y)');
	// 'a => b => c => a(c)(b(c))'
	// f('λa.λb.λc.((a c) (b c))'); // Combinator S
	// f('λa.(λb.(a (b b)) λb.(a (b b)))'); // Combinator Y (fixed-point; used to implement recursion)

	expect(() => f('(x y')).toThrow(SyntaxException);

	f('1');
	// f('[+ 2 3]');
});
