// tom-weatherhead/thaw-grammar/test/languages/lambda-calculus/lambda-calculus.test.ts

// From https://opendsa.cs.vt.edu/ODSA/Books/PL/html/ChurchNumerals.html :

// TRUE = λx.λy.x
//
// FALSE = λx.λy.y
//
// IF = λb.λx.λy.((b x) y)
//
// AND = λp.λq.((p q) FALSE)
//
// OR = λp.λq.(((IF p) TRUE) q)
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

import { LanguageSelector, LexicalAnalyzerSelector, ParserSelector } from 'thaw-interpreter-types';

import { createTokenizer } from 'thaw-lexical-analyzer';

import { createParser, SyntaxException } from 'thaw-parser';

import {
	areIsomorphic,
	BetaReductionStrategy,
	createGrammar,
	ILCExpression,
	ILCVariable
} from '../../..';

const ls = LanguageSelector.LambdaCalculus;

test('LambdaCalculus parser instance creation test', () => {
	// Arrange
	const grammar = createGrammar(ls);

	// Act
	const parser = createParser(ParserSelector.LL1, grammar);

	// Assert
	expect(parser).toBeTruthy();
});

test('LambdaCalculus recognize test', () => {
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
	f('λa.λb.λc.((a c) (b c))'); // Combinator S
	f('λa.(λb.(a (b b)) λb.(a (b b)))'); // Combinator Y (fixed-point; used to implement recursion)

	expect(() => f('(x y')).toThrow(SyntaxException);
});

function getParseFunction(): (str: string) => ILCExpression {
	const grammar = createGrammar(ls);
	const tokenizer = createTokenizer(LexicalAnalyzerSelector.MidnightHack, ls);
	const parser = createParser(ParserSelector.LL1, grammar);

	return (str: string) => parser.parse(tokenizer.tokenize(str)) as ILCExpression;
}

test('LambdaCalculus parse test', () => {
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

function getfb(fparam?: (str: string) => ILCExpression): (s: string) => ILCExpression {
	const generateNewVariableName = createVariableNameGenerator();
	const f = typeof fparam !== 'undefined' ? fparam : getParseFunction();
	const maxBetaReductionDepth = 10;
	const fb = (s: string): ILCExpression =>
		f(s).betaReduce(
			BetaReductionStrategy.CallByName,
			generateNewVariableName,
			maxBetaReductionDepth
		);

	return fb;
}

test('LambdaCalculus Variable Name Generator test', () => {
	const generateNewVariableName = createVariableNameGenerator();

	expect(generateNewVariableName()).toBe('v1');
	expect(generateNewVariableName()).toBe('v2');
	expect(generateNewVariableName()).toBe('v3');
});

test('LambdaCalculus beta-reduction test 1', () => {
	// Arrange
	// const generateNewVariableName = createVariableNameGenerator();
	// const f = getParseFunction();
	const fb = getfb();

	// const expr = f('(λx.x y)');
	// const reducedExpr = expr.betaReduce(
	// 	BetaReductionStrategy.CallByName,
	// 	generateNewVariableName,
	// 	10
	// );
	const reducedExpr = fb('(λx.x y)');

	// expect(expr).toBeTruthy();
	expect(reducedExpr).toBeTruthy();

	const variableName = (reducedExpr as ILCVariable).name;

	expect(variableName).toBeDefined();
	expect(variableName).toBe('y');
});

test('LambdaCalculus beta-reduction test 2', () => {
	// Arrange
	// const generateNewVariableName = createVariableNameGenerator();
	// const f = getParseFunction();
	const fb = getfb();

	// const expr = f('(λf.λx.x g)');
	//
	// expect(expr).toBeTruthy();
	//
	// const reducedExpr = expr.betaReduce(
	// 	BetaReductionStrategy.CallByName,
	// 	generateNewVariableName,
	// 	10
	// );
	const reducedExpr = fb('(λf.λx.x g)');

	expect(reducedExpr).toBeTruthy();
	expect(reducedExpr.toString()).toBe('λx.x');
});

test('LambdaCalculus beta-reduction test 3', () => {
	// Arrange
	// const generateNewVariableName = createVariableNameGenerator();
	// const f = getParseFunction();
	const fb = getfb();

	// const expr = f('((λf.λx.x g) h)');
	//
	// expect(expr).toBeTruthy();
	//
	// const reducedExpr = expr.betaReduce(
	// 	BetaReductionStrategy.CallByName,
	// 	generateNewVariableName,
	// 	10
	// );
	const reducedExpr = fb('((λf.λx.x g) h)');

	expect(reducedExpr).toBeTruthy();
	expect(reducedExpr.toString()).toBe('h');
});

test('LambdaCalculus expression unification test 1', () => {
	// Arrange
	const f = getParseFunction();

	// Act
	const expr1 = f('x');
	const expr2 = f('y');
	const unifyingSubstitution = expr1.unify(expr2);

	console.log(`unifyingSubstitution is: ${unifyingSubstitution}`);

	// Assert
	expect(unifyingSubstitution).toBeDefined();
	expect(unifyingSubstitution.toString()).toBe('[x -> y]');
	expect(areIsomorphic(expr1, expr2)).toBe(true);
});

test('LambdaCalculus Church Numerals Successor Test 1', () => {
	// Arrange

	// To encode the non-negative integers, Church used the following encoding:

	// ZERO = λf.λ x.x
	const strZero = 'λf.λx.x';

	// A successor function SUCC = λn.λf.λx.(f((nf)x))
	const strSucc = 'λn.λf.λx.(f ((n f) x))';

	// ONE = (SUCC ZERO) = λf.λ x.(fx)
	const strOneExpected1 = 'λf.λx.(f x)';
	const strOneExpected = 'λv1.λv2.(v1 v2)';
	const strOneSrc = `(${strSucc} ${strZero})`;

	// TWO = (SUCC ONE) = λf.λ x.(f(fx))
	const strTwoExpected1 = 'λf.λx.(f (f x))';
	const strTwoExpected = 'λv3.λv4.(v3 (v3 v4))';
	const strTwoSrc = `(${strSucc} ${strOneExpected1})`;

	// THREE = (SUCC TWO) = λf.λ x.(f(f(fx)))
	// const strThreeExpected1 = 'λf.λx.(f (f (f x)))';
	const strThreeExpected = 'λv5.λv6.(v5 (v5 (v5 v6)))';
	const strThreeSrc = `(${strSucc} ${strTwoExpected1})`;

	// const generateNewVariableName = createVariableNameGenerator();
	const f = getParseFunction();
	const fb = getfb(f);

	// Act

	// const zero = f(strZero);
	// const succ = f(strSucc);
	// const one = f(strOneSrc);
	// const two = f(strTwoSrc);
	// const three = f(strThreeSrc);

	// const oneActual = one.betaReduce(BetaReductionStrategy.CallByName, generateNewVariableName, 10);
	const oneActual = fb(strOneSrc);
	const strOneActual = oneActual.toString();
	// const twoActual = two.betaReduce(BetaReductionStrategy.CallByName, generateNewVariableName, 10);
	const twoActual = fb(strTwoSrc);
	const strTwoActual = twoActual.toString();
	// const threeActual = three.betaReduce(
	// 	BetaReductionStrategy.CallByName,
	// 	generateNewVariableName,
	// 	10
	// );
	const threeActual = fb(strThreeSrc);
	const strThreeActual = threeActual.toString();

	// Assert
	expect(strOneActual).toBe(strOneExpected);
	expect(strTwoActual).toBe(strTwoExpected);
	expect(strThreeActual).toBe(strThreeExpected);

	expect(areIsomorphic(oneActual, f(strOneExpected))).toBe(true);
	expect(areIsomorphic(twoActual, f(strTwoExpected))).toBe(true);
	expect(areIsomorphic(threeActual, f(strThreeExpected))).toBe(true);
});

test('LambdaCalculus Church Numerals Predecessor Test 1', () => {
	expect(0).toBe(0);

	// Arrange

	// We add a Church encoding for an operation that computes the predecessor of a Church numeral n:
	//
	const strPred = 'λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)';

	const strZero = 'λf.λx.x';
	const strOne = 'λf.λx.(f x)';
	const strTwo = 'λf.λx.(f (f x))';
	const strThree = 'λf.λx.(f (f (f x)))';

	// const generateNewVariableName = createVariableNameGenerator();
	const f = getParseFunction();
	// const maxBetaReductionDepth = 10;
	// const fb = (s: string): ILCExpression => f(s).betaReduce(
	// 	BetaReductionStrategy.CallByName,
	// 	generateNewVariableName,
	// 	maxBetaReductionDepth
	// );
	const fb = getfb(f);

	// Act
	const zeroExpected = f(strZero);
	const oneExpected = f(strOne);
	const twoExpected = f(strTwo);

	const twoActual = fb(`(${strPred} ${strThree})`);
	const oneActual = fb(`(${strPred} ${strTwo})`);
	const zeroActual = fb(`(${strPred} ${strOne})`);

	// Assert
	expect(areIsomorphic(twoActual, twoExpected)).toBe(true);
	expect(areIsomorphic(oneActual, oneExpected)).toBe(true);
	expect(areIsomorphic(zeroActual, zeroExpected)).toBe(true);
});

test('LambdaCalculus Church Numerals And Test 1', () => {
	expect(0).toBe(0);

	// Arrange
	const strTrue = 'λx.λy.x';
	const strFalse = 'λx.λy.y';

	// AND = λp.λq.((p q) FALSE)
	const strAnd = `λp.λq.((p q) ${strFalse})`;

	const f = getParseFunction();
	const fb = getfb(f);

	const tt = f(strTrue);
	const ff = f(strFalse);

	// Act
	const fAndf = fb(`((${strAnd} ${strFalse}) ${strFalse})`);
	const fAndt = fb(`((${strAnd} ${strFalse}) ${strTrue})`);
	const tAndf = fb(`((${strAnd} ${strTrue}) ${strFalse})`);
	const tAndt = fb(`((${strAnd} ${strTrue}) ${strTrue})`);

	// Assert

	console.log(`ff is ${ff}`);
	console.log(`tt is ${tt}`);

	const u1 = ff.unify(tt);

	console.log(`ff.unify(tt) is ${u1}`);

	// ff is λx.λy.y
	// tt is λx.λy.x
	// ff.unify(tt) is [y -> x]
	// WRONG: We cannot replace y with x in either λx.λy.y or λx.λy.x because
	// x already occurs in each.
	// We need to expand our 'occurs' check to check both entire expressions,
	// not just the parts of the two expressions that have not yet been unified.

	expect(areIsomorphic(ff, tt)).toBe(false);	// Fails. TODO: Debug.
	expect(areIsomorphic(tt, ff)).toBe(false);	// Fails. TODO: Debug.
	expect(areIsomorphic(ff, ff)).toBe(true);
	expect(areIsomorphic(tt, tt)).toBe(true);

	expect(areIsomorphic(fAndf, ff)).toBe(true);
	expect(areIsomorphic(fAndf, tt)).toBe(false);

	expect(areIsomorphic(fAndt, ff)).toBe(true);
	expect(areIsomorphic(fAndt, tt)).toBe(false);

	expect(areIsomorphic(tAndf, ff)).toBe(true);
	expect(areIsomorphic(tAndf, tt)).toBe(false);	// Fails. TODO: Debug.

	expect(areIsomorphic(tAndt, ff)).toBe(false);	// Fails. TODO: Debug.
	expect(areIsomorphic(tAndt, tt)).toBe(true);
});

test('LambdaCalculus Church Numerals Or Test 1', () => {
	expect(0).toBe(0);

	// IF = λb.λx.λy.((b x) y)
	//
	// OR = λp.λq.(((IF p) TRUE) q)

	const strTrue = 'λx.λy.x';
});

test('LambdaCalculus Church Numerals Multiplication Test 1', () => {
	expect(0).toBe(0);

	// MULT = λm.λn.λf.(m(nf))
});

test('LambdaCalculus Church Numerals isZero Test 1', () => {
	expect(0).toBe(0);

	// And finally, we add an operation to test for zero, which can be used in the if-then-else you identified in the previous practice problem (see above).
	//
	// ISZERO = λn.((n λx.FALSE) TRUE)

	// const f = getParseFunction();
	//
	// const expr = f('(λx.x y)');
	// const reducedExpr = expr.betaReduce(
	// 	BetaReductionStrategy.CallByName,
	// 	generateNewVariableName,
	// 	10
	// );

	// Arrange
	const strTrue = 'λx.λy.x';
	const strFalse = 'λx.λy.y';
	const strIsZero = `λn.((n λx.${strFalse}) ${strTrue})`;

	const strZero = 'λf.λx.x';
	const strOne = 'λf.λx.(f x)';
	const strTwo = 'λf.λx.(f (f x))';

	const f = getParseFunction();
	// const generateNewVariableName = createVariableNameGenerator();
	const fb = getfb(f);

	// Act
	const tt = f(strTrue);
	const ff = f(strFalse);
	const exprIsZeroZero = fb(`(${strIsZero} ${strZero})`);
	const exprIsOneZero = fb(`(${strIsZero} ${strOne})`);
	const exprIsTwoZero = fb(`(${strIsZero} ${strTwo})`);

	// console.log(`exprIsZeroZero: ${exprIsZeroZero}`);
	// console.log(`exprIsOneZero: ${exprIsOneZero}`);
	// console.log(`exprIsTwoZero: ${exprIsTwoZero}`);
	//
	// console.log(`TRUE: ${tt}`);
	// console.log(`FALSE: ${ff}`);
	//
	// const s1 = exprIsZeroZero.unify(tt);
	// const s2 = exprIsZeroZero.unify(ff);

	// console.log(`exprIsZeroZero.unify(TRUE): ${s1}`);
	// console.log(`exprIsZeroZero.unify(FALSE): ${s2}`);

	// Assert
	expect(exprIsZeroZero.isIsomorphicTo(tt)).toBe(true);
	expect(exprIsZeroZero.isIsomorphicTo(ff)).toBe(false);

	expect(exprIsOneZero.isIsomorphicTo(tt)).toBe(false);
	expect(exprIsOneZero.isIsomorphicTo(ff)).toBe(true);

	expect(exprIsTwoZero.isIsomorphicTo(tt)).toBe(false);
	expect(exprIsTwoZero.isIsomorphicTo(ff)).toBe(true);
});
