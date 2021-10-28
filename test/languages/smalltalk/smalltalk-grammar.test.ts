// tom-weatherhead/thaw-grammar/test/languages/smalltalk/smalltalk-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { createTokenizer } from 'thaw-lexical-analyzer';

import { createParser /*, SyntaxException */ } from 'thaw-parser';

import { createGrammar } from '../../..';

const ls = LanguageSelector.Smalltalk;

test('SmalltalkGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(ls);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});

test('SmalltalkGrammar parser instance creation test', () => {
	// Arrange
	const grammar = createGrammar(ls);

	// Act
	const parser = createParser(grammar.defaultParser, grammar);

	// Assert
	expect(parser).toBeTruthy();
});

test('SmalltalkGrammar recognize test', () => {
	// Arrange
	const grammar = createGrammar(ls);
	// const tokenizer = createTokenizer(LexicalAnalyzerSelector.MidnightHack, ls);
	const tokenizer = createTokenizer(grammar.defaultLexicalAnalyzer, ls);
	const parser = createParser(grammar.defaultParser, grammar);

	const f = (str: string): void => parser.recognize(tokenizer.tokenize(str));

	const str1 = [
		'(class Rand Object ()',
		'    (seed)',
		'    (define init () (begin (initRand self 1) self))',
		'    (define initRand (n) (set seed n))',
		'    (define nextRand () (set seed (mod (+ (* seed 9) 5) 1024)))',
		')'
	].join(' ');

	f(str1);
	f('(set r (init (new Rand)))');
	f('(nextRand r)');
	f('(nextRand r)');
	// f('');

	f('0');
	f('x');
	f('(define add (x y) (+ x y))');
	f('(+ 2 3)');

	// expect(() => f('')).toThrow(SyntaxException);
});
