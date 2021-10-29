// tom-weatherhead/thaw-grammar/test/languages/clu/clu-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { createParser /*, SyntaxException */ } from 'thaw-parser';

import { createGrammar } from '../../..';

import { createInfrastructure } from '../../create-infrastructure';

const ls = LanguageSelector.CLU;

test('CLUGrammar instance creation test', () => {
	// Arrange
	// const grammar = createGrammar(ls);
	const { grammar } = createInfrastructure(ls);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});

test('CLUGrammar parser instance creation test', () => {
	// Arrange
	// const grammar = createGrammar(ls);

	// Act
	// const parser = createParser(grammar.defaultParser, grammar);
	const { parser } = createInfrastructure(ls);

	// Assert
	expect(parser).toBeTruthy();
});

// test('CLUGrammar recognize test', () => {
// 	// Arrange
// 	const { tokenizer, parser } = createInfrastructure(ls);
//
// 	const f = (str: string): void => parser.recognize(tokenizer.tokenize(str));
//
// 	// f('(define fac (n) (*/ (indx n)))'); // Page 70
// 	// f('(define avg (v) (/ (+/ v) (shape v)))');
// 	// f('(define neg (v) (- 0 v))');
// 	// f('(define min (v1 v2) (neg (max (neg v1) (neg v2))))');
// 	// f('(define min/ (v) (neg (max/ (neg v))))');
// 	// f('(define not (x) (- 1 x))'); // Page 71
// 	// f('(define <> (x y) (not (= x y)))');
// 	// f('(define signum (x) (+ (* (< x 0) -1) (> x 0)))'); // Page 72
//
// 	// expect(() => f('')).toThrow(SyntaxException);
// });
