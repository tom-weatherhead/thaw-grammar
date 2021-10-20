// tom-weatherhead/thaw-grammar/test/languages/lambda-calculus-augmented-syntax/lambda-calculus-augmented-syntax.test.ts

'use strict';

import { ifDefinedThenElse } from 'thaw-common-utilities.ts';

import { LanguageSelector, LexicalAnalyzerSelector, ParserSelector } from 'thaw-interpreter-types';

import { createTokenizer } from 'thaw-lexical-analyzer';

import { createParser, SyntaxException } from 'thaw-parser';

import {
	areIsomorphic,
	BetaReductionStrategy,
	churchNumeralToInteger,
	createCombinator,
	// createEmptyList,
	// createFunctionAppend,
	// createFunctionGetHeadOfList,
	// createFunctionGetTailOfList,
	// createFunctionIsListEmpty,
	createGrammar,
	// createMapOfLCExprNamesToExprs,
	createOperatorDecrementUsage,
	createOperatorIncrementUsage,
	createOperatorIsZeroUsage,
	createValueFalse,
	createValueTrue,
	createVariableNameGenerator,
	// defaultMaxBetaReductionDepth,
	getfb2,
	ILCExpression,
	ILCVariable,
	integerToChurchNumeral,
	lcaCons,
	lcaCreateNil,
	lcaHead,
	lcaIsNull,
	// , lcaIsNullUsage
	lcaTail
	// , mapCombinatorNamesToStrings
	// , mapLCExprNamesToStrings
} from '../../..';

const ls = LanguageSelector.LambdaCalculusWithAugmentedSyntax;

test('LambdaCalculusWithAugmentedSyntax Grammar instance creation test', () => {
	// Arrange
	// Act
	const grammar = createGrammar(ls);

	// Assert
	expect(grammar).toBeTruthy();
});

test('LambdaCalculusWithAugmentedSyntax parser instance creation test', () => {
	// Arrange
	const grammar = createGrammar(ls);

	// Act
	const parser = createParser(ParserSelector.LL1, grammar);

	// Assert
	expect(parser).toBeTruthy();
});

test('LambdaCalculusWithAugmentedSyntax recognize test', () => {
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
	f('let y = λx.x in y');

	expect(() => f('(x y')).toThrow(SyntaxException);
});

function getParseFunction(): (str: string) => ILCExpression {
	const grammar = createGrammar(ls);
	const tokenizer = createTokenizer(LexicalAnalyzerSelector.MidnightHack, ls);
	const parser = createParser(ParserSelector.LL1, grammar);

	return (str: string) => parser.parse(tokenizer.tokenize(str)) as ILCExpression;
}

test('LambdaCalculusWithAugmentedSyntax parse test', () => {
	// Arrange
	const f = getParseFunction();

	expect(f('x')).toBeTruthy();
	expect(f('(x y)')).toBeTruthy();
	expect(f('λx.x')).toBeTruthy();
	expect(f('(λx.x y)')).toBeTruthy();
});

function getfb(
	fparam?: (str: string) => ILCExpression,
	options: {
		strategy?: BetaReductionStrategy;
		generateNewVariableName?: () => string;
		maxDepth?: number;
	} = {}
): (s: string) => ILCExpression {
	// const generateNewVariableName = createVariableNameGenerator();
	const f = typeof fparam !== 'undefined' ? fparam : getParseFunction();
	// const fb = (s: string): ILCExpression => f(s).betaReduce(options);
	const fb = (s: string): ILCExpression =>
		f(s).betaReduce({
			strategy: options.strategy,
			generateNewVariableName: ifDefinedThenElse(
				options.generateNewVariableName,
				createVariableNameGenerator()
			),
			maxDepth: options.maxDepth
		});

	return fb;
}

test('LambdaCalculusWithAugmentedSyntax Variable Name Generator test', () => {
	const generateNewVariableName = createVariableNameGenerator();

	expect(generateNewVariableName()).toBe('v1');
	expect(generateNewVariableName()).toBe('v2');
	expect(generateNewVariableName()).toBe('v3');
});

test('LambdaCalculusWithAugmentedSyntax beta-reduction test 1', () => {
	// Arrange
	const fb = getfb();
	const reducedExpr = fb('(λx.x y)');

	expect(reducedExpr).toBeTruthy();

	const variableName = (reducedExpr as ILCVariable).name;

	expect(variableName).toBeDefined();
	expect(variableName).toBe('y');
});

test('LambdaCalculusWithAugmentedSyntax beta-reduction test 2', () => {
	// Arrange
	const fb = getfb();
	const reducedExpr = fb('(λf.λx.x g)');

	expect(reducedExpr).toBeTruthy();
	expect(reducedExpr.toString()).toBe('λx.x');
});

test('LambdaCalculusWithAugmentedSyntax beta-reduction test 3', () => {
	// Arrange
	const fb = getfb();
	const reducedExpr = fb('((λf.λx.x g) h)');

	expect(reducedExpr).toBeTruthy();
	expect(reducedExpr.toString()).toBe('h');
});

test('LambdaCalculusWithAugmentedSyntax expression unification test 1', () => {
	// Arrange
	const f = getParseFunction();

	// Act
	const expr1 = f('x');
	const expr2 = f('y');
	const unifyingSubstitution = expr1.unify(expr2);

	// console.log(`unifyingSubstitution is: ${unifyingSubstitution}`);

	// Assert
	expect(unifyingSubstitution).toBeDefined();
	expect(unifyingSubstitution.toString()).toBe('[x -> y]');
	expect(areIsomorphic(expr1, expr2)).toBe(true);
});

test('LambdaCalculusWithAugmentedSyntax If Test 1', () => {
	// Arrange
	const conditionValue = createValueTrue();
	const thenValue = 2;
	const elseValue = 3;

	const fb = getfb();

	// Act
	const expr = fb(
		`(if ${conditionValue} ${integerToChurchNumeral(thenValue)} ${integerToChurchNumeral(
			elseValue
		)})`
	);

	// Assert
	expect(churchNumeralToInteger(expr)).toBe(thenValue);
});

test('LambdaCalculusWithAugmentedSyntax If Test 2', () => {
	// Arrange
	const conditionValue = createValueFalse();
	const thenValue = 2;
	const elseValue = 3;

	const fb = getfb();

	// Act
	const expr = fb(
		`(if ${conditionValue} ${integerToChurchNumeral(thenValue)} ${integerToChurchNumeral(
			elseValue
		)})`
	);

	// Assert
	expect(churchNumeralToInteger(expr)).toBe(elseValue);
});

test('LambdaCalculusWithAugmentedSyntax If True Test 1', () => {
	// Arrange
	const conditionValue = 'true';
	const thenValue = 2;
	const elseValue = 3;

	const fb = getfb();

	// Act
	const expr = fb(
		`(if ${conditionValue} ${integerToChurchNumeral(thenValue)} ${integerToChurchNumeral(
			elseValue
		)})`
	);

	// Assert
	expect(churchNumeralToInteger(expr)).toBe(thenValue);
});

test('LambdaCalculusWithAugmentedSyntax If False Test 1', () => {
	// Arrange
	const conditionValue = 'false';
	const thenValue = 2;
	const elseValue = 3;

	const fb = getfb();

	// Act
	const expr = fb(
		`(if ${conditionValue} ${integerToChurchNumeral(thenValue)} ${integerToChurchNumeral(
			elseValue
		)})`
	);

	// Assert
	expect(churchNumeralToInteger(expr)).toBe(elseValue);
});

test('LambdaCalculusWithAugmentedSyntax AND Truth Table Test 1', () => {
	// Arrange
	const t = createValueTrue();
	const f = createValueFalse();

	const fb = getfb();

	// Act
	const actualResult0 = fb('(and false false)');
	const actualResult1 = fb('(and false true)');
	const actualResult2 = fb('(and true false)');
	const actualResult3 = fb('(and true true)');

	// Assert
	expect(areIsomorphic(actualResult0, f)).toBe(true);
	expect(areIsomorphic(actualResult0, t)).toBe(false);

	expect(areIsomorphic(actualResult1, f)).toBe(true);
	expect(areIsomorphic(actualResult1, t)).toBe(false);

	expect(areIsomorphic(actualResult2, f)).toBe(true);
	expect(areIsomorphic(actualResult2, t)).toBe(false);

	expect(areIsomorphic(actualResult3, f)).toBe(false);
	expect(areIsomorphic(actualResult3, t)).toBe(true);
});

test('LambdaCalculusWithAugmentedSyntax OR Truth Table Test 1', () => {
	// Arrange
	const t = createValueTrue();
	const f = createValueFalse();

	const fb = getfb();

	// Act
	const actualResult0 = fb('(or false false)');
	const actualResult1 = fb('(or false true)');
	const actualResult2 = fb('(or true false)');
	const actualResult3 = fb('(or true true)');

	// Assert
	expect(areIsomorphic(actualResult0, f)).toBe(true);
	expect(areIsomorphic(actualResult0, t)).toBe(false);

	expect(areIsomorphic(actualResult1, f)).toBe(false);
	expect(areIsomorphic(actualResult1, t)).toBe(true);

	expect(areIsomorphic(actualResult2, f)).toBe(false);
	expect(areIsomorphic(actualResult2, t)).toBe(true);

	expect(areIsomorphic(actualResult3, f)).toBe(false);
	expect(areIsomorphic(actualResult3, t)).toBe(true);
});

test('LambdaCalculusWithAugmentedSyntax Let Test 1', () => {
	// Arrange
	const variableName = 'x';
	const variableValue = createValueTrue();
	const expr = `((${variableName} ${variableName}) ${variableName})`;

	const fb = getfb();

	// Act
	const expr1 = fb(`let ${variableName} = ${variableValue} in ${expr}`);
	const expr2 = fb(`(λ${variableName}.${expr} ${variableValue})`);

	// Assert
	expect(areIsomorphic(expr1, expr2)).toBe(true);
});

test('LambdaCalculusWithAugmentedSyntax Church Numerals Successor Test 1', () => {
	// Arrange

	// To encode the non-negative integers, Church used the following encoding:
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
	const strThreeExpected = 'λv5.λv6.(v5 (v5 (v5 v6)))';
	const strThreeSrc = `(${strSucc} ${strTwoExpected1})`;

	const f = getParseFunction();
	const fb = getfb(f);

	// Act
	const oneActual = fb(strOneSrc);
	const twoActual = fb(strTwoSrc);
	const threeActual = fb(strThreeSrc);

	// Assert
	expect(areIsomorphic(oneActual, f(strOneExpected))).toBe(true);
	expect(areIsomorphic(twoActual, f(strTwoExpected))).toBe(true);
	expect(areIsomorphic(threeActual, f(strThreeExpected))).toBe(true);
});

test('LambdaCalculusWithAugmentedSyntax Church Numerals Predecessor Test 1', () => {
	// Arrange

	// We add a Church encoding for an operation that computes the predecessor of a Church numeral n:
	const strPred = 'λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)';

	const strZero = 'λf.λx.x';
	const strOne = 'λf.λx.(f x)';
	const strTwo = 'λf.λx.(f (f x))';
	const strThree = 'λf.λx.(f (f (f x)))';

	const f = getParseFunction();
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

test('LambdaCalculusWithAugmentedSyntax Church Numerals And Test 1', () => {
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
	expect(areIsomorphic(ff, tt)).toBe(false);
	expect(areIsomorphic(tt, ff)).toBe(false);
	expect(areIsomorphic(ff, ff)).toBe(true);
	expect(areIsomorphic(tt, tt)).toBe(true);

	expect(areIsomorphic(fAndf, ff)).toBe(true);
	expect(areIsomorphic(fAndf, tt)).toBe(false);

	expect(areIsomorphic(fAndt, ff)).toBe(true);
	expect(areIsomorphic(fAndt, tt)).toBe(false);

	expect(areIsomorphic(tAndf, ff)).toBe(true);
	expect(areIsomorphic(tAndf, tt)).toBe(false);

	expect(areIsomorphic(tAndt, ff)).toBe(false);
	expect(areIsomorphic(tAndt, tt)).toBe(true);
});

test('LambdaCalculusWithAugmentedSyntax Church Numerals Or Test 1', () => {
	expect(0).toBe(0);

	// Arrange
	const strTrue = 'λx.λy.x';
	const strFalse = 'λx.λy.y';

	// IF = λb.λx.λy.((b x) y)
	const strIf = 'λb.λx.λy.((b x) y)';

	// OR = λp.λq.(((IF p) TRUE) q)
	const strOr = `λp.λq.(((${strIf} p) ${strTrue}) q)`;

	const f = getParseFunction();
	const fb = getfb(f);

	const tt = f(strTrue);
	const ff = f(strFalse);

	// Act
	const fOrf = fb(`((${strOr} ${strFalse}) ${strFalse})`);
	const fOrt = fb(`((${strOr} ${strFalse}) ${strTrue})`);
	const tOrf = fb(`((${strOr} ${strTrue}) ${strFalse})`);
	const tOrt = fb(`((${strOr} ${strTrue}) ${strTrue})`);

	// Assert
	expect(areIsomorphic(ff, tt)).toBe(false);
	expect(areIsomorphic(tt, ff)).toBe(false);
	expect(areIsomorphic(ff, ff)).toBe(true);
	expect(areIsomorphic(tt, tt)).toBe(true);

	expect(areIsomorphic(fOrf, ff)).toBe(true);
	expect(areIsomorphic(fOrf, tt)).toBe(false);

	expect(areIsomorphic(fOrt, ff)).toBe(false);
	expect(areIsomorphic(fOrt, tt)).toBe(true);

	expect(areIsomorphic(tOrf, ff)).toBe(false);
	expect(areIsomorphic(tOrf, tt)).toBe(true);

	expect(areIsomorphic(tOrt, ff)).toBe(false);
	expect(areIsomorphic(tOrt, tt)).toBe(true);
});

test('LambdaCalculusWithAugmentedSyntax Church Numerals Addition Test 1', () => {
	// Addition and multiplication can be encoded as curried functions:
	//
	// PLUS = λm.λn.λf.λx.((n f) ((m f) x))

	// Arrange
	const strTwo = 'λf.λx.(f (f x))';
	const strThree = 'λf.λx.(f (f (f x)))';
	const strFive = 'λf.λx.(f (f (f (f (f x)))))';

	const strPlus = 'λm.λn.λf.λx.((n f) ((m f) x))';

	const f = getParseFunction();
	const fb = getfb(f);

	const expectedResult = f(strFive);

	// Act
	const actualResult = fb(`((${strPlus} ${strTwo}) ${strThree})`);

	// Assert
	expect(actualResult.isIsomorphicTo(expectedResult)).toBe(true);
});

test('LambdaCalculusWithAugmentedSyntax Church Numerals Multiplication Test 1', () => {
	// MULT = λm.λn.λf.(m (n f))

	// Arrange
	const strTwo = 'λf.λx.(f (f x))';
	const strThree = 'λf.λx.(f (f (f x)))';
	const strSix = 'λf.λx.(f (f (f (f (f (f x))))))';

	const strMult = 'λm.λn.λf.(m (n f))';

	const f = getParseFunction();
	const fb = getfb(f);

	const expectedResult = f(strSix);

	// Act
	const actualResult = fb(`((${strMult} ${strTwo}) ${strThree})`);

	// Assert
	expect(actualResult.isIsomorphicTo(expectedResult)).toBe(true);
});

test('LambdaCalculusWithAugmentedSyntax Church Numerals isZero Test 1', () => {
	// And finally, we add an operation to test for zero, which can be used in the if-then-else you identified in the previous practice problem (see above).
	//
	// ISZERO = λn.((n λx.FALSE) TRUE)

	// Arrange
	const strTrue = 'λx.λy.x';
	const strFalse = 'λx.λy.y';
	const strIsZero = `λn.((n λx.${strFalse}) ${strTrue})`;

	const strZero = 'λf.λx.x';
	const strOne = 'λf.λx.(f x)';
	const strTwo = 'λf.λx.(f (f x))';

	const f = getParseFunction();
	const fb = getfb(f);

	// Act
	const tt = f(strTrue);
	const ff = f(strFalse);
	const exprIsZeroZero = fb(`(${strIsZero} ${strZero})`);
	const exprIsOneZero = fb(`(${strIsZero} ${strOne})`);
	const exprIsTwoZero = fb(`(${strIsZero} ${strTwo})`);

	// Assert
	expect(exprIsZeroZero.isIsomorphicTo(tt)).toBe(true);
	expect(exprIsZeroZero.isIsomorphicTo(ff)).toBe(false);

	expect(exprIsOneZero.isIsomorphicTo(tt)).toBe(false);
	expect(exprIsOneZero.isIsomorphicTo(ff)).toBe(true);

	expect(exprIsTwoZero.isIsomorphicTo(tt)).toBe(false);
	expect(exprIsTwoZero.isIsomorphicTo(ff)).toBe(true);
});

test('LambdaCalculusWithAugmentedSyntax Y combinator test 1', () => {
	// Arrange

	// const strG = 'λr.λn.if (= n 0) 1 (* n (r (- n 1)))';

	// Rewrite strG as pure λ-calculus:

	const f = getParseFunction();

	const strTrue = `${createValueTrue()}`;
	const strFalse = `${createValueFalse()}`;
	const strIf = 'λb.λx.λy.((b x) y)';
	const one = integerToChurchNumeral(1);
	const strOne = `${one}`;
	// const strTwo = 'λf.λx.(f (f x))';
	const three = integerToChurchNumeral(3);
	const strThree = `${three}`;
	const six = integerToChurchNumeral(6);
	const strSix = `${six}`;
	const strIsZero = `λn.((n λx.${strFalse}) ${strTrue})`;
	const strMult = 'λm.λn.λf.(m (n f))';
	const strPredecessor = 'λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)';

	const strG = `λr.λn.(((${strIf} (${strIsZero} n)) ${strOne}) ((${strMult} n) (r (${strPredecessor} n))))`;

	// ((* 2) 3) is isomorphic to 6 via the CallByName strategy only:
	// const expr = `((${strMult} ${strTwo}) ${strThree})`;

	// This Y combinator test succeeds via the CallByName strategy only:
	const expr = [
		`let y = ${createCombinator('Y')} in`,
		`let g = ${strG} in`,
		`((y g) ${strThree})` // 3 factorial
	].join(' ');

	expect(f(expr)).toBeDefined();

	// Act
	// const generateNewVariableName = createVariableNameGenerator();
	// const maxBetaReductionDepth = 100;

	const expectedResult = f(strSix);

	const successes: number[] = [];

	const actualResult1 = getfb(f, { strategy: BetaReductionStrategy.CallByName })(expr);

	// console.log(`Y combinator test: CallByName yields ${actualResult1}`);

	if (actualResult1.isIsomorphicTo(expectedResult)) {
		successes.push(101);
	}

	const actualResult8 = getfb(f, { strategy: BetaReductionStrategy.ThAWHackForYCombinator })(
		expr
	);

	// console.log(`Y combinator test: ThAWHackForYCombinator yields ${actualResult8}`);

	if (actualResult8.isIsomorphicTo(expectedResult)) {
		successes.push(108);
	}

	// console.log('Y combinator test 1: successes:', successes);

	// Assert
	expect(successes.length > 0).toBe(true);
	// expect(actualResult.isIsomorphicTo(expectedResult)).toBe(true);
});

test('LambdaCalculusWithAugmentedSyntax Y combinator test 2', () => {
	// Arrange
	const f = getParseFunction();

	const strG = 'λr.λn.(if (zero? n) 1 (* n (r (dec n))))';
	const three = integerToChurchNumeral(3);

	// This Y combinator test succeeds via the CallByName strategy only:
	const expr = [
		`let y = ${createCombinator('Y')} in`,
		`let g = ${strG} in`,
		`((y g) ${three})` // 3 factorial
	].join(' ');

	const expectedResult = 6;

	expect(f(expr)).toBeDefined(); // Ensure that it parses

	// Act

	// const actualResult1 = churchNumeralToInteger(getfb(f, { strategy: BetaReductionStrategy.CallByName })(expr));
	//
	// expect(actualResult1).toBe(expectedResult);

	const actualResult2 = churchNumeralToInteger(
		getfb(f, { strategy: BetaReductionStrategy.NormalOrder })(expr)
	);

	expect(actualResult2).toBe(expectedResult);

	// const actualResult3 = churchNumeralToInteger(getfb(f, { strategy: BetaReductionStrategy.CallByValue })(expr));
	//
	// expect(actualResult3).toBe(expectedResult);

	const actualResult4 = churchNumeralToInteger(
		getfb(f, { strategy: BetaReductionStrategy.ApplicativeOrder })(expr)
	);

	expect(actualResult4).toBe(expectedResult);

	const actualResult5 = churchNumeralToInteger(
		getfb(f, { strategy: BetaReductionStrategy.HybridApplicativeOrder })(expr)
	);

	expect(actualResult5).toBe(expectedResult);

	// const actualResult6 = churchNumeralToInteger(
	// 	getfb(f, { strategy: BetaReductionStrategy.HeadSpine })(expr)
	// );
	//
	// expect(actualResult6).toBe(expectedResult);

	// const actualResult7 = churchNumeralToInteger(
	// 	getfb(f, { strategy: BetaReductionStrategy.HybridNormalOrder })(expr)
	// );
	//
	// expect(actualResult7).toBe(expectedResult);

	const actualResult8 = churchNumeralToInteger(
		getfb(f, { strategy: BetaReductionStrategy.ThAWHackForYCombinator })(expr)
	);

	expect(actualResult8).toBe(expectedResult);
});

test('LambdaCalculusWithAugmentedSyntax integerToChurchNumeral Test 1', () => {
	expect(integerToChurchNumeral(0).toString()).toBe('λf.λx.x');
	expect(integerToChurchNumeral(1).toString()).toBe('λf.λx.(f x)');
	expect(integerToChurchNumeral(2).toString()).toBe('λf.λx.(f (f x))');
	expect(integerToChurchNumeral(3).toString()).toBe('λf.λx.(f (f (f x)))');
});

test('LambdaCalculusWithAugmentedSyntax churchNumeralToInteger Test 1', () => {
	// Arrange
	const f = getParseFunction();

	// Act
	// Assert
	expect(Number.isNaN(churchNumeralToInteger(f('x')))).toBe(true);
	expect(Number.isNaN(churchNumeralToInteger(f('λx.x')))).toBe(true);
	expect(Number.isNaN(churchNumeralToInteger(f('(f x)')))).toBe(true);

	expect(churchNumeralToInteger(f('λf.λx.x'))).toBe(0);
	expect(churchNumeralToInteger(f('λf.λx.(f x)'))).toBe(1);
	expect(churchNumeralToInteger(f('λf.λx.(f (f x))'))).toBe(2);
	expect(churchNumeralToInteger(f('λf.λx.(f (f (f x)))'))).toBe(3);
});

test('LambdaCalculusWithAugmentedSyntax Church Numeral Addition Test 2', () => {
	// Arrange
	const x = 2;
	const y = 3;
	const expectedResult = x + y;

	const fb = getfb();

	// Act
	const expr = fb(`(+ ${integerToChurchNumeral(x)} ${integerToChurchNumeral(y)})`);
	const actualResult = churchNumeralToInteger(expr);

	// Assert
	expect(actualResult).toBe(expectedResult);
});

test('LambdaCalculusWithAugmentedSyntax Church Numeral Multiplication Test 2', () => {
	// Arrange
	const x = 2;
	const y = 3;
	const expectedResult = x * y;

	const fb = getfb();

	// Act
	const expr = fb(`(* ${integerToChurchNumeral(x)} ${integerToChurchNumeral(y)})`);
	const actualResult = churchNumeralToInteger(expr);

	// Assert
	expect(actualResult).toBe(expectedResult);
});

test('LambdaCalculusWithAugmentedSyntax Church Numeral Increment Test 1', () => {
	// Arrange
	const fn = (n: number): ILCExpression =>
		createOperatorIncrementUsage(integerToChurchNumeral(n)).betaReduce({
			generateNewVariableName: createVariableNameGenerator()
		});

	// Act
	const actualResult0 = fn(0);
	const actualResult1 = fn(1);
	const actualResult2 = fn(2);
	const actualResult3 = fn(3);

	// Assert
	expect(churchNumeralToInteger(actualResult0)).toBe(1);
	expect(churchNumeralToInteger(actualResult1)).toBe(2);
	expect(churchNumeralToInteger(actualResult2)).toBe(3);
	expect(churchNumeralToInteger(actualResult3)).toBe(4);
});

test('LambdaCalculusWithAugmentedSyntax Church Numeral Increment Test 2', () => {
	// Arrange
	const grammar = createGrammar(ls);
	const tokenizer = createTokenizer(LexicalAnalyzerSelector.MidnightHack, ls);
	const parser = createParser(ParserSelector.LL1, grammar);
	const fb2 = getfb2(tokenizer, parser, {
		generateNewVariableName: createVariableNameGenerator()
	});
	const fn = (n: number): number => churchNumeralToInteger(fb2(`(inc ${n})`));

	// Act
	const actualResult0 = fn(0);
	const actualResult1 = fn(1);
	const actualResult2 = fn(2);
	const actualResult3 = fn(3);

	// Assert
	expect(actualResult0).toBe(1);
	expect(actualResult1).toBe(2);
	expect(actualResult2).toBe(3);
	expect(actualResult3).toBe(4);
});

test('LambdaCalculusWithAugmentedSyntax Church Numeral Decrement Test 1', () => {
	// Arrange
	const fn = (n: number): ILCExpression =>
		createOperatorDecrementUsage(integerToChurchNumeral(n)).betaReduce({
			generateNewVariableName: createVariableNameGenerator()
		});

	// Act
	const actualResult1 = fn(1);
	const actualResult2 = fn(2);
	const actualResult3 = fn(3);

	// Assert
	expect(churchNumeralToInteger(actualResult1)).toBe(0);
	expect(churchNumeralToInteger(actualResult2)).toBe(1);
	expect(churchNumeralToInteger(actualResult3)).toBe(2);
});

test('LambdaCalculusWithAugmentedSyntax Church Numeral Decrement Test 2', () => {
	// Arrange
	const grammar = createGrammar(ls);
	const tokenizer = createTokenizer(LexicalAnalyzerSelector.MidnightHack, ls);
	const parser = createParser(ParserSelector.LL1, grammar);
	const fb2 = getfb2(tokenizer, parser, {
		generateNewVariableName: createVariableNameGenerator()
	});
	const fn = (n: number): number => churchNumeralToInteger(fb2(`(dec ${n})`));

	// Act
	const actualResult1 = fn(1);
	const actualResult2 = fn(2);
	const actualResult3 = fn(3);

	// Assert
	expect(actualResult1).toBe(0);
	expect(actualResult2).toBe(1);
	expect(actualResult3).toBe(2);
});

test('LambdaCalculusWithAugmentedSyntax Church Numeral isZero Predicate Test 1', () => {
	// Arrange
	const t = createValueTrue();
	const f = createValueFalse();

	const fn = (n: number): ILCExpression =>
		createOperatorIsZeroUsage(integerToChurchNumeral(n)).betaReduce({
			generateNewVariableName: createVariableNameGenerator()
		});

	// Act
	const actualResult0 = fn(0);
	const actualResult1 = fn(1);
	const actualResult2 = fn(2);
	const actualResult3 = fn(3);

	// Assert
	expect(areIsomorphic(actualResult0, t)).toBe(true);
	expect(areIsomorphic(actualResult0, f)).toBe(false);

	expect(areIsomorphic(actualResult1, t)).toBe(false);
	expect(areIsomorphic(actualResult1, f)).toBe(true);

	expect(areIsomorphic(actualResult2, t)).toBe(false);
	expect(areIsomorphic(actualResult2, f)).toBe(true);

	expect(areIsomorphic(actualResult3, t)).toBe(false);
	expect(areIsomorphic(actualResult3, f)).toBe(true);
});

test('LambdaCalculusWithAugmentedSyntax Church Numeral isZero Predicate Test 2', () => {
	// Arrange
	const t = createValueTrue();
	const f = createValueFalse();
	const grammar = createGrammar(ls);
	const tokenizer = createTokenizer(LexicalAnalyzerSelector.MidnightHack, ls);
	const parser = createParser(ParserSelector.LL1, grammar);
	const fb2 = getfb2(tokenizer, parser, {
		generateNewVariableName: createVariableNameGenerator()
	});
	const fn = (n: number): ILCExpression => fb2(`(zero? ${n})`);

	// Act
	const actualResult0 = fn(0);
	const actualResult1 = fn(1);
	const actualResult2 = fn(2);
	const actualResult3 = fn(3);

	// Assert
	expect(areIsomorphic(actualResult0, t)).toBe(true);
	expect(areIsomorphic(actualResult0, f)).toBe(false);

	expect(areIsomorphic(actualResult1, t)).toBe(false);
	expect(areIsomorphic(actualResult1, f)).toBe(true);

	expect(areIsomorphic(actualResult2, t)).toBe(false);
	expect(areIsomorphic(actualResult2, f)).toBe(true);

	expect(areIsomorphic(actualResult3, t)).toBe(false);
	expect(areIsomorphic(actualResult3, f)).toBe(true);
});

function reducesToTrue(str: string): boolean {
	return areIsomorphic(getfb()(str), createValueTrue());
}

function reducesToFalse(str: string): boolean {
	return areIsomorphic(getfb()(str), createValueFalse());
}

// test('LambdaCalculusWithAugmentedSyntax List Test 1', () => {
// 	// Arrange
// 	// const fb = getfb();
// 	//
// 	// const t = createValueTrue();
// 	// const f = createValueFalse();
//
// 	const emptyList = createEmptyList();
// 	// createFunctionAppend
// 	// createFunctionGetHeadOfList
// 	// createFunctionGetTailOfList
// 	// createFunctionIsListEmpty
// 	const fnIsListEmpty = createFunctionIsListEmpty();
//
// 	// Act
//
// 	// Assert
// 	expect(reducesToTrue(`(${fnIsListEmpty} ${emptyList})`)).toBe(true);
// });
//
// test('LambdaCalculusWithAugmentedSyntax List Test 2', () => {
// 	const emptyList = createEmptyList({ f: 'f0', x: 'x0' });
// 	const element1AsInt = 2;
// 	const element1 = integerToChurchNumeral(element1AsInt, { f: 'f1', x: 'x1' });
// 	const fnAppend = createFunctionAppend({ f: 'f2', x: 'x2' });
// 	const fb = getfb();
//
// 	// console.log(`LCAug List Test 2: emptyList is: ${emptyList}`);
// 	// console.log(`LCAug List Test 2: element1 is: ${element1}`);
//
// 	const list1 = fb(`((${fnAppend} ${element1}) ${emptyList})`);
//
// 	console.log(`LCAug List Test 2: list1 is: ${list1}`);
//
// 	const fnGetHeadOfList = createFunctionGetHeadOfList();
// 	const head1 = fb(`(${fnGetHeadOfList} ${list1})`);
//
// 	// console.log(`LCAug List Test 2: head1 is: ${head1}`);
//
// 	// const fnGetTailOfList = createFunctionGetTailOfList({ l: 'l4', a: 'a4', b: 'b4' });
// 	// const tail1 = fb(`(${fnGetTailOfList} ${list1})`);
//
// 	// console.log(`LCAug List Test 2: tail1 is: ${tail1}`);
//
// 	const fnIsListEmpty = createFunctionIsListEmpty();
//
// 	// Assert
// 	expect(areIsomorphic(head1, element1)).toBe(true);
// 	expect(churchNumeralToInteger(head1)).toBe(element1AsInt);
// 	expect(reducesToFalse(`(${fnIsListEmpty} ${list1})`)).toBe(true);
// 	// expect(reducesToTrue(`(${fnIsListEmpty} ${tail1})`)).toBe(true);
// });

test('LambdaCalculusWithAugmentedSyntax List Version 2 Test 1', () => {
	// Arrange
	const emptyList = lcaCreateNil({ f: 'f0', x: 'x0', y: 'y0' });
	const fnIsListEmpty = lcaIsNull({ l: 'l1', h: 'h1', t: 't1', x: 'x1', y: 'y1' });

	// Act

	// Assert
	expect(reducesToTrue(`(${fnIsListEmpty} ${emptyList})`)).toBe(true);
});

test('LambdaCalculusWithAugmentedSyntax List Version 2 Test 2', () => {
	const emptyList = lcaCreateNil({ f: 'f0', x: 'x0', y: 'y0' });
	const element1AsInt = 2;
	const element1 = integerToChurchNumeral(element1AsInt, { f: 'f1', x: 'x1' });
	const fnAppend = lcaCons({ h: 'h2', t: 't2', f: 'f2' });
	const fb = getfb();

	console.log(`LCAug List Test 2: emptyList is: ${emptyList}`);
	console.log(`LCAug List Test 2: element1 is: ${element1}`);

	const list1 = fb(`((${fnAppend} ${element1}) ${emptyList})`);

	console.log(`LCAug List Test 2: list1 is: ${list1}`);

	const fnGetHeadOfList = lcaHead({ l: 'l3', h: 'h3', t: 't3' });
	const head1 = fb(`(${fnGetHeadOfList} ${list1})`);

	// console.log(`LCAug List Test 2: head1 is: ${head1}`);

	const fnGetTailOfList = lcaTail({ l: 'l4', h: 'h4', t: 't4' });
	const tail1 = fb(`(${fnGetTailOfList} ${list1})`);

	// console.log(`LCAug List Test 2: tail1 is: ${tail1}`);

	const fnIsListEmpty = lcaIsNull({ l: 'l5', h: 'h5', t: 't5', x: 'x5', y: 'y5' });

	// Assert
	expect(areIsomorphic(head1, element1)).toBe(true);
	expect(churchNumeralToInteger(head1)).toBe(element1AsInt);
	expect(reducesToFalse(`(${fnIsListEmpty} ${list1})`)).toBe(true);
	expect(reducesToTrue(`(${fnIsListEmpty} ${tail1})`)).toBe(true);
});

function exprToString(l: ILCExpression): string {
	const n = churchNumeralToInteger(l);

	return Number.isNaN(n) ? `${l}` : `${n}`;
}

function listToString(l: ILCExpression): string {
	const fnIsListEmpty = lcaIsNull({ l: 'l0', h: 'h0', t: 't0', x: 'x0', y: 'y0' });

	const fnGetHeadOfList = lcaHead({ l: 'l3', h: 'h3', t: 't3' });
	const fnGetTailOfList = lcaTail({ l: 'l4', h: 'h4', t: 't4' });

	const fb = getfb();
	const strList: string[] = [];

	while (!reducesToTrue(`(${fnIsListEmpty} ${l})`)) {
		const head = fb(`(${fnGetHeadOfList} ${l})`);

		strList.unshift(exprToString(head));
		l = fb(`(${fnGetTailOfList} ${l})`);
	}

	return '[' + strList.join(', ') + ']';
}

test('LambdaCalculusWithAugmentedSyntax List Version 2 Test 3', () => {
	const element1AsInt = 2;
	const element2AsInt = 3;
	const element3AsInt = 5;

	const emptyList = lcaCreateNil({ f: 'f0', x: 'x0', y: 'y0' });
	const element1 = integerToChurchNumeral(element1AsInt, { f: 'f1', x: 'x1' });
	const element2 = integerToChurchNumeral(element2AsInt, { f: 'f2', x: 'x2' });
	const element3 = integerToChurchNumeral(element3AsInt, { f: 'f3', x: 'x3' });
	const fnCons = lcaCons({ h: 'h4', t: 't4', f: 'f4' });
	const fb = getfb();

	const list1 = fb(`((${fnCons} ${element1}) ${emptyList})`);
	const list2 = fb(`((${fnCons} ${element2}) ${list1})`);
	const list3 = fb(`((${fnCons} ${element3}) ${list2})`);

	// const fnGetHeadOfList = lcaHead({ l: 'l3', h: 'h3', t: 't3' });
	// const head1 = fb(`(${fnGetHeadOfList} ${list1})`);
	//
	// const fnGetTailOfList = lcaTail({ l: 'l4', h: 'h4', t: 't4' });
	// const tail1 = fb(`(${fnGetTailOfList} ${list1})`);
	//
	// const fnIsListEmpty = lcaIsNull({ l: 'l5', h: 'h5', t: 't5', x: 'x5', y: 'y5' });

	const str = listToString(list3);

	// Assert
	// expect(areIsomorphic(head1, element1)).toBe(true);
	// expect(churchNumeralToInteger(head1)).toBe(element1AsInt);
	// expect(reducesToFalse(`(${fnIsListEmpty} ${list1})`)).toBe(true);
	// expect(reducesToTrue(`(${fnIsListEmpty} ${tail1})`)).toBe(true);
	expect(str).toBe(`[${element1AsInt}, ${element2AsInt}, ${element3AsInt}]`);
});
