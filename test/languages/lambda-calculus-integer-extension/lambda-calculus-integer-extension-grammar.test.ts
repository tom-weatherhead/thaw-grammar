//

'use strict';

import { LanguageSelector, LexicalAnalyzerSelector, ParserSelector } from 'thaw-interpreter-types';

import { createTokenizer } from 'thaw-lexical-analyzer';

import { createGrammar, ILCExpression, isLCIntegerLiteral, LCIntegerLiteral } from '../../..';

import { createParser, SyntaxException } from 'thaw-parser';

const ls = LanguageSelector.LambdaCalculusIntegerExtension;

const strTrue = 'λx.λy.x'; // This is identical to the K combinator
const strFalse = 'λx.λy.y';

const strCombinatorI = 'λx.x';
const strCombinatorK = 'λx.λy.x';
const strCombinatorS = 'λx.λy.λz.((x z) (y z))';
const strCombinatorY = 'λg.(λx.(g (x x)) λx.(g (x x)))';

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

	f(strTrue);
	f(strFalse);

	f(strCombinatorI);
	f(strCombinatorK);
	f(strCombinatorS);
	f(strCombinatorY);
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

function createVariableNameGenerator(): () => string {
	let n = 0;

	return () => `v${++n}`;
}

test('LambdaCalculusIntegerExtensionGrammar integer test', () => {
	const f = getParseFunction();
	const seven = f('7');

	expect(`${seven}`).toBe('7');
	expect(isLCIntegerLiteral(seven)).toBeTruthy();
	expect((seven as LCIntegerLiteral).value).toBe(7);
});

test('LambdaCalculusIntegerExtensionGrammar integer addition test', () => {
	const f = getParseFunction();
	const eight = f('[+ 3 5]').deltaReduce();

	expect(`${eight}`).toBe('8');
	expect(isLCIntegerLiteral(eight)).toBeTruthy();
	expect((eight as LCIntegerLiteral).value).toBe(8);
});

test('LambdaCalculusIntegerExtensionGrammar if test', () => {
	const f = getParseFunction();
	const variableNameGenerator = createVariableNameGenerator();

	const actualResult1 = f('if [= 0 0] 7 13').deltaReduce().betaReduce(variableNameGenerator);
	const actualResult2 = f('if [= 0 1] 7 13').deltaReduce().betaReduce(variableNameGenerator);

	expect(isLCIntegerLiteral(actualResult1)).toBeTruthy();
	expect(isLCIntegerLiteral(actualResult2)).toBeTruthy();
	expect((actualResult1 as LCIntegerLiteral).value).toBe(7);
	expect((actualResult2 as LCIntegerLiteral).value).toBe(13);
});

test('LambdaCalculusIntegerExtensionGrammar Y combinator test', () => {
	const f = getParseFunction();
	const strG = 'λr.λn.if [= n 0] 1 [* n (r [- n 1])]';

	expect(f(strG)).toBeTruthy();

	const strFactorial5 = `((${strCombinatorY} ${strG}) 5)`;

	expect(f(strFactorial5)).toBeTruthy();
});
