// tom-weatherhead/thaw-grammar/test/languages/smalltalk/smalltalk-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

// import { createParser /*, SyntaxException */ } from 'thaw-parser';
//
import {
	EnvironmentFrame,
	ISmalltalkExpression,
	ISmalltalkValue,
	SmalltalkGlobalInfo
} from '../../..';

import {
	createFnParser,
	createFnRecognizer,
	createInfrastructure
} from '../../create-infrastructure';

const ls = LanguageSelector.Smalltalk;

test('SmalltalkGrammar instance creation test', () => {
	// Arrange
	const { grammar } = createInfrastructure(ls);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});

test('SmalltalkGrammar parser instance creation test', () => {
	// Arrange
	// Act
	const { parser } = createInfrastructure(ls);

	// Assert
	expect(parser).toBeTruthy();
});

// Sample Smalltalk code (using Kamin's syntax) :

// ; Random.txt - A pseudorandom number generator
// ; Exercise 1 on page 344
// (class Rand Object ()
//     (seed)
//     (define init () (begin (initRand self 1) self))
//     (define initRand (n) (set seed n))
//     (define nextRand () (set seed (mod (+ (* seed 9) 5) 1024)))
// )
// (set r (init (new Rand)))
// (nextRand r)
// (nextRand r)

test('SmalltalkGrammar recognize test', () => {
	// Arrange
	// const { tokenizer, parser } = createInfrastructure(ls);
	//
	// const f = (str: string): void => parser.recognize(tokenizer.tokenize(str));
	const f = createFnRecognizer(ls);

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

test('SmalltalkGrammar addition test', () => {
	const f = createFnParser<ISmalltalkExpression>(ls);
	const a = 2;
	const b = 3;

	const localEnvironment = new EnvironmentFrame<ISmalltalkValue>();
	const globalInfo = new SmalltalkGlobalInfo();

	const actualSmalltalkExpression = f(`(+ ${a} ${b})`);
	const actualSmalltalkValue = actualSmalltalkExpression.evaluate(localEnvironment, globalInfo);

	console.log('actualSmalltalkValue is', actualSmalltalkValue);

	expect(actualSmalltalkValue.isInteger).toBe(true);
	expect(actualSmalltalkValue.value).toBe(a + b);
});
