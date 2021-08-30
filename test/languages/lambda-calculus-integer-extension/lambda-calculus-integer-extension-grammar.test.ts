//

'use strict';

import { LanguageSelector, LexicalAnalyzerSelector, ParserSelector } from 'thaw-interpreter-types';

import { createTokenizer } from 'thaw-lexical-analyzer';

import { createGrammar, ILCExpression, isLCIntegerLiteral, LCIntegerLiteral } from '../../..';

import { createParser, SyntaxException } from 'thaw-parser';

// const ls = LanguageSelector.LambdaCalculus;
const ls = LanguageSelector.LambdaCalculusIntegerExtension;

test('LambdaCalculusIntegerExtensionGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(ls);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});

test('LambdaCalculusIntegerExtensionGrammar recognize test', () => {
	// Arrange
	const grammar = createGrammar(ls);
	// const tokenizer = createTokenizer(LexicalAnalyzerSelector.MidnightHack, ls);
	const tokenizer = createTokenizer(LexicalAnalyzerSelector.MidnightHack, ls);
	const parser = createParser(ParserSelector.LL1, grammar);

	const f = (str: string): void => parser.recognize(tokenizer.tokenize(str));

	// For a list of combinators, see https://github.com/loophp/combinator ,
	// all (?) of which can be implemented in terms of just S and K.

	// Act
	// Assert
	f('x');
	f('λx.x'); // Combinator I (identity) === ((S K) K)
	f('λx.λy.x'); // Combinator K
	f('(x y)');
	f('(λx.x y)');
	// 'a => b => c => a(c)(b(c))'
	expect(() => f('λa')).toThrow(SyntaxException);
	expect(() => f('λa.')).toThrow(SyntaxException);
	expect(() => f('λa.b.λc.')).toThrow(SyntaxException);
	f('λa.λb.λc.((a c) (b c))'); // Combinator S
	f('λa.(λb.(a (b b)) λb.(a (b b)))'); // Combinator Y (fixed-point; used to implement recursion)

	expect(() => f('(x y')).toThrow(SyntaxException);

	f('1');
	f('[+ 2 3]');
});

function getParseFunction(): (str: string) => ILCExpression {
	const grammar = createGrammar(ls);
	const tokenizer = createTokenizer(LexicalAnalyzerSelector.MidnightHack, ls);
	const parser = createParser(ParserSelector.LL1, grammar);

	return (str: string) => parser.parse(tokenizer.tokenize(str)) as ILCExpression;
}

test('LambdaCalculusIntegerExtensionGrammar parse test', () => {
	// Arrange
	const f = getParseFunction();

	expect(f('x')).toBeTruthy();
	expect(f('(x y)')).toBeTruthy();
	expect(f('λx.x')).toBeTruthy();
	expect(f('(λx.x y)')).toBeTruthy();
});

// function createVariableNameGenerator(): () => string {
// 	let n = 0;
//
// 	return () => `v${++n}`;
// }

test('LambdaCalculusIntegerExtensionGrammar integer test', () => {
	const f = getParseFunction();
	const seven = f('7');

	expect(`${seven}`).toBe('7');
	expect(isLCIntegerLiteral(seven)).toBeTruthy();
	expect((seven as LCIntegerLiteral).value).toBe(7);
});
