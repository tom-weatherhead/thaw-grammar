// tom-weatherhead/thaw-grammar/test/languages/apl/apl-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { createParser /*, SyntaxException */ } from 'thaw-parser';

import { createGrammar } from '../../..';

import { createInfrastructure } from '../../create-infrastructure';

const ls = LanguageSelector.APL;

test('APLGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(ls);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});

test('APLGrammar parser instance creation test', () => {
	// Arrange
	const grammar = createGrammar(ls);

	// Act
	const parser = createParser(grammar.defaultParser, grammar);

	// Assert
	expect(parser).toBeTruthy();
});

test('APLGrammar recognize test', () => {
	// Arrange
	const { tokenizer, parser } = createInfrastructure(ls);

	const f = (str: string): void => parser.recognize(tokenizer.tokenize(str));

	// parser.Recognize(tokenizer.Tokenize("(define fac (n) (*/ (indx n)))"));                     // Page 70
	f('(define fac (n) (*/ (indx n)))'); // Page 70
	// parser.Recognize(tokenizer.Tokenize("(define avg (v) (/ (+/ v) (shape v)))"));
	f('(define avg (v) (/ (+/ v) (shape v)))');
	// parser.Recognize(tokenizer.Tokenize("(define neg (v) (- 0 v))"));
	f('(define neg (v) (- 0 v))');
	// parser.Recognize(tokenizer.Tokenize("(define min (v1 v2) (neg (max (neg v1) (neg v2))))"));
	f('(define min (v1 v2) (neg (max (neg v1) (neg v2))))');
	// parser.Recognize(tokenizer.Tokenize("(define min/ (v) (neg (max/ (neg v))))"));
	f('(define min/ (v) (neg (max/ (neg v))))');
	// parser.Recognize(tokenizer.Tokenize("(define not (x) (- 1 x))"));                           // Page 71
	f('(define not (x) (- 1 x))'); // Page 71
	// parser.Recognize(tokenizer.Tokenize("(define <> (x y) (not (= x y)))"));
	f('(define <> (x y) (not (= x y)))');
	// parser.Recognize(tokenizer.Tokenize("(define signum (x) (+ (* (< x 0) -1) (> x 0)))"));     // Page 72
	f('(define signum (x) (+ (* (< x 0) -1) (> x 0)))'); // Page 72

	// const str1 = [
	// 	'(class Rand Object ()',
	// 	'    (seed)',
	// 	'    (define init () (begin (initRand self 1) self))',
	// 	'    (define initRand (n) (set seed n))',
	// 	'    (define nextRand () (set seed (mod (+ (* seed 9) 5) 1024)))',
	// 	')'
	// ].join(' ');
	//
	// f(str1);
	// f('(set r (init (new Rand)))');
	// f('(nextRand r)');
	// f('(nextRand r)');
	// // f('');
	//
	// f('0');
	// f('x');
	// f('(define add (x y) (+ x y))');
	// f('(+ 2 3)');

	// expect(() => f('')).toThrow(SyntaxException);
});
