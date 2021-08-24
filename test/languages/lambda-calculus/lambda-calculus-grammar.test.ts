// tom-weatherhead/thaw-grammar/test/languages/lambda-calculus/lambda-calculus-grammar.test.ts

// From https://opendsa.cs.vt.edu/ODSA/Books/PL/html/ChurchNumerals.html :

// TRUE = λx.λy.x
//
// FALSE = λx.λy.y
//
// AND = λp.λq.((pq) FALSE)
//
// OR = λp.λq.(((IF p) TRUE) q)
//
// IF = λb.λx.λy.((b x) y)
//
// To encode the non-negative integers, Church used the following encoding:
//
// ZERO = λf.λ x.x
//
// A successor function SUCC = λn.λf.λx.(f((nf)x))
//
// ONE = (SUCC ZERO) = λf.λ x.(fx)
//
// TWO = (SUCC ONE) = λf.λ x.(f(fx))
//
// THREE = (SUCC TWO) = λf.λ x.(f(f(fx)))
//
// FOUR = (SUCC THREE) = ???

// Addition and multiplication can be encoded as curried functions:
//
// PLUS = λm.λn.λf.λx.((nf)((mf)x))
//
// MULT = λm.λn.λf.(m(nf))
//
// We add a Church encoding for an operation that computes the predecessor of a Church numeral n:
//
// PRED = λn.λf.λx.(((nλg.λh.(h(gf)))λu.x)λu.u)
//
// And finally, we add an operation to test for zero, which can be used in the if-then-else you identified in the previous practice problem (see above).
//
// ISZERO = λn.((nλx.FALSE)TRUE)

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { createGrammar } from '../../..';

test('LambdaCalculusGrammar instance creation test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.LambdaCalculus);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});

test('LambdaCalculusGrammar recognize test', () => {
	// Arrange
	const grammar = createGrammar(LanguageSelector.LambdaCalculus);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});
