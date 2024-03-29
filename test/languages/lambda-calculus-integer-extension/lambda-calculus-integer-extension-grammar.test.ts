// tom-weatherhead/thaw-grammar/test/languages/lambda-calculus-integer-extension/lambda-calculus-integer-extension.test.ts

'use strict';

import { LanguageSelector, LexicalAnalyzerSelector, ParserSelector } from 'thaw-interpreter-types';

import { createTokenizer } from 'thaw-lexical-analyzer';

import {
	BetaReductionStrategy,
	createGrammar,
	// getfb2,
	ILCExpression,
	isLCFunctionCall,
	isLCIntegerLiteral,
	// isLCPrimitiveOperator,
	LCFunctionCall,
	LCIntegerLiteral,
	LCLambdaExpression,
	LCPrimitiveOperator // ,
	// mapCombinatorNamesToStrings,
	// mapLCExprNamesToStrings
} from '../../..';

import { createParser, SyntaxException } from 'thaw-parser';

const ls = LanguageSelector.LambdaCalculusIntegerExtension;

const strTrue = 'λx.λy.x'; // This is identical to the K combinator
const strFalse = 'λx.λy.y';

const strCombinatorI = 'λx.x';
const strCombinatorK = 'λx.λy.x';
const strCombinatorS = 'λx.λy.λz.((x z) (y z))';
const strCombinatorY = 'λg.(λx.(g (x x)) λx.(g (x x)))';
// const strCombinatorY = mapCombinatorNamesToStrings.get('Y');

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
	f('(+ 2 3)');

	f(strTrue);
	f(strFalse);

	f(strCombinatorI);
	f(strCombinatorK);
	f(strCombinatorS);
	expect(strCombinatorY).toBeDefined();

	if (typeof strCombinatorY !== 'undefined') {
		f(strCombinatorY);
	}
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
	const eight = f('(+ 3 5)').deltaReduce();

	expect(`${eight}`).toBe('8');
	expect(isLCIntegerLiteral(eight)).toBeTruthy();
	expect((eight as LCIntegerLiteral).value).toBe(8);
});

test('LambdaCalculusIntegerExtensionGrammar integer subtraction test', () => {
	const f = getParseFunction();
	const three = f('(- 8 5)').deltaReduce();

	expect(`${three}`).toBe('3');
	expect(isLCIntegerLiteral(three)).toBeTruthy();
	expect((three as LCIntegerLiteral).value).toBe(3);
});

test('LambdaCalculusIntegerExtensionGrammar integer multiplication test', () => {
	const f = getParseFunction();
	const ninetyOne = f('(* 7 13)').deltaReduce();

	expect(`${ninetyOne}`).toBe('91');
	expect(isLCIntegerLiteral(ninetyOne)).toBeTruthy();
	expect((ninetyOne as LCIntegerLiteral).value).toBe(91);
});

test('LambdaCalculusIntegerExtensionGrammar if test', () => {
	const f = getParseFunction();
	// const variableNameGenerator = createVariableNameGenerator();
	const options = {
		strategy: BetaReductionStrategy.CallByName,
		generateNewVariableName: createVariableNameGenerator()
	};

	const actualResult1 = f('if (= 0 0) 7 13').deltaReduce().betaReduce(options);
	const actualResult2 = f('if (= 0 1) 7 13').deltaReduce().betaReduce(options);

	expect(isLCIntegerLiteral(actualResult1)).toBeTruthy();
	expect(isLCIntegerLiteral(actualResult2)).toBeTruthy();
	expect((actualResult1 as LCIntegerLiteral).value).toBe(7);
	expect((actualResult2 as LCIntegerLiteral).value).toBe(13);
});

function yCombinatorPretest1Reducer(
	actualResult2: LCFunctionCall,
	variableNameGenerator: () => string
): LCIntegerLiteral {
	// const actualResult2 = actualResult1.betaReduce(
	// 	BetaReductionStrategy.CallByName,
	// 	variableNameGenerator,
	// 	2
	// ) as LCFunctionCall;

	// console.log(`Y combinator pre-test 1: actualResult2 is: ${actualResult2};`, actualResult2);

	// LCFunctionCall.betaReduceCore() : lambdaExpressionBody is (λx.(g (x x)) λx.(g (x x)))

	// LCFunctionCall.betaReduceCore() : Replaced g with:
	// λr.λn.(([= n 0]  1) [* n (r [- n 1] )] );
	// bodyAfterSubst is (λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x)) λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x)))

	// actualResult2 is: ((λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x)) λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x))) 1)

	expect(actualResult2).toBeTruthy();

	if (!isLCFunctionCall(actualResult2)) {
		console.error(
			`yCombinatorPretest1Reducer() : actualResult2 is ${typeof actualResult2} ${actualResult2}`
		);
	}

	expect(isLCFunctionCall(actualResult2)).toBeTruthy();

	if (!isLCFunctionCall((actualResult2 as LCFunctionCall).callee)) {
		console.error(
			`yCombinatorPretest1Reducer() : (actualResult2 as LCFunctionCall).callee is ${
				(actualResult2 as LCFunctionCall).callee
			}`
		);
	}

	expect(isLCFunctionCall((actualResult2 as LCFunctionCall).callee)).toBeTruthy();

	const f3 = (actualResult2 as LCFunctionCall).callee as LCFunctionCall;

	// console.log(`Y combinator pre-test 1: f3 is: ${f3};`, f3);

	// f3 is: (λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1] )] ) (x x)) λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)))

	// Compare f9 vs. f3:
	// const f9 = (actualResult8.rightChild as LCFunctionCall).callee as LCFunctionCall;

	// const actualResult3 = f3.betaReduce(BetaReductionStrategy.CallByName, variableNameGenerator, 1);
	//
	// console.log(`Y combinator pre-test 1: actualResult3 is: ${actualResult3};`, actualResult3);

	// actualResult3 is: (λv2.λv3.(([= v3 0]  1) [* v3 (v2 [- v3 1] )] ) (λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x)) λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x))))

	// const actualResult3a = f3.betaReduceCore(
	// 	f3.callee as LCLambdaExpression,
	// 	f3.arg,
	// 	variableNameGenerator
	// ) as LCFunctionCall;
	const actualResult3a = f3.betaReduce({
		strategy: BetaReductionStrategy.CallByName,
		generateNewVariableName: variableNameGenerator
	}) as LCFunctionCall;

	// console.log(`Y combinator pre-test 1: actualResult3a is: ${actualResult3a};`, actualResult3a);

	// actualResult3a is: (λv2.λv3.(([= v3 0]  1) [* v3 (v2 [- v3 1] )] ) (λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x)) λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x))))

	// lambdaExpressionBody is (λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x))

	// Replaced x with λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x))

	// actualResult3a is: (λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x)) λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x))))

	// const actualResult4 = actualResult3a.betaReduceCore(
	// 	actualResult3a.callee as LCLambdaExpression,
	// 	actualResult3a.arg,
	// 	variableNameGenerator
	// ) as LCLambdaExpression;
	const actualResult4 = actualResult3a.betaReduce({
		strategy: BetaReductionStrategy.CallByName,
		generateNewVariableName: variableNameGenerator
	}) as LCLambdaExpression;

	// console.log(`Y combinator pre-test 1: actualResult4 is: ${actualResult4};`, actualResult4);
	// console.log(
	// 	`Y combinator pre-test 1: actualResult2.arg is: ${actualResult2.arg};`,
	// 	actualResult2.arg
	// );

	// actualResult4 is: λn.(([= n 0] 1) [* n ((λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)) λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x))) [- n 1])])

	const actualResult5 = actualResult4.body.substituteForUnboundVariable(
		actualResult4.arg.name,
		actualResult2.arg
	);

	// console.log(`Y combinator pre-test 1: actualResult5 is: ${actualResult5};`, actualResult5);

	// actualResult5 is: (([= 1 0] 1) [* 1 ((λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)) λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x))) [- 1 1])])

	const actualResult6 = actualResult5.deltaReduce() as LCFunctionCall;

	// console.log(`Y combinator pre-test 1: actualResult6 is: ${actualResult6};`, actualResult6);

	// actualResult6 is: ((λx.λy.y 1) [* 1 ((λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)) λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x))) 0)])

	const actualResult7 = actualResult6.callee.betaReduce(
		{
			strategy: BetaReductionStrategy.CallByName,
			generateNewVariableName: variableNameGenerator
		}
		// BetaReductionStrategy.CallByName,
		// variableNameGenerator,
		// 10
	);

	// console.log(`Y combinator pre-test 1: actualResult7 is: ${actualResult7};`, actualResult7);

	// actualResult7 is: λy.y

	// const actualResult8 = actualResult6.betaReduceCore(
	// 	actualResult7 as LCLambdaExpression,
	// 	actualResult6.arg,
	// 	variableNameGenerator
	// ) as LCIntegerLiteral | LCPrimitiveOperator;
	const actualResult8 = actualResult7.betaReduce({
		strategy: BetaReductionStrategy.CallByName,
		generateNewVariableName: variableNameGenerator
	}) as LCIntegerLiteral | LCPrimitiveOperator;

	// console.log(`Y combinator pre-test 1: actualResult8 is: ${actualResult8};`, actualResult8);

	// actualResult8 is: [* 1 ((λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)) λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x))) 0)]

	// vs. actualResult2 is: ((λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x)) λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x))) 1)

	let finalResult: LCIntegerLiteral;

	if (isLCIntegerLiteral(actualResult8)) {
		finalResult = actualResult8;
	} else {
		// Compare f9 vs. f3
		// const f9 = (actualResult8.rightChild as LCFunctionCall).callee as LCFunctionCall;
		const f9 = actualResult8.rightChild as LCFunctionCall;

		const actualResult9 = yCombinatorPretest1Reducer(f9, variableNameGenerator);

		if (!isLCIntegerLiteral(actualResult9)) {
			throw new Error('yCombinatorPretest1Reducer returned a non-LCIntegerLiteral');
		}

		// finalResult = actualResult9;

		// TODO: Replace actualResult14 with actualResult9

		// actualResult8.rightChild === actualResult14 === 1

		// expect(actualResult9).toBeTruthy();
		// expect(isLCIntegerLiteral(actualResult9)).toBeTruthy();
		// expect((actualResult9 as LCIntegerLiteral).value).toBe(1);

		const f15 = new LCPrimitiveOperator(
			actualResult8.name,
			actualResult8.leftChild,
			actualResult9
		);
		const actualResult15 = f15.deltaReduce();

		if (!isLCIntegerLiteral(actualResult15)) {
			throw new Error('yCombinatorPretest1Reducer: actualResult15 is a non-LCIntegerLiteral');
		}

		finalResult = actualResult15;
	}

	return finalResult;
}

function yCombinatorPretest1ReducerLauncher(n: number): LCIntegerLiteral {
	const f = getParseFunction();
	const variableNameGenerator = createVariableNameGenerator();

	const strG = 'λr.λn.if (= n 0) 1 (* n (r (- n 1)))';

	expect(strCombinatorY).toBeDefined();

	if (typeof strCombinatorY === 'undefined') {
		throw new Error('strCombinatorY is undefined');
	}

	const strFactorial1 = `((${strCombinatorY} ${strG}) ${n})`;

	const actualResult1 = f(strFactorial1);

	const actualResult2 = actualResult1.betaReduce(
		{ generateNewVariableName: variableNameGenerator }
		// BetaReductionStrategy.CallByName,
		// variableNameGenerator,
		// 2
	) as LCFunctionCall;

	if (isLCIntegerLiteral(actualResult2)) {
		return actualResult2;
	}

	return yCombinatorPretest1Reducer(actualResult2, variableNameGenerator);
}

test('LambdaCalculusIntegerExtensionGrammar Y combinator pre-test 1', () => {
	const f = getParseFunction();
	const variableNameGenerator = createVariableNameGenerator();

	const strG = 'λr.λn.if (= n 0) 1 (* n (r (- n 1)))';

	expect(f(strG)).toBeTruthy();

	// const n = 0;
	// const nFactorial = 1;

	// const n = 1;
	// const nFactorial = 1;

	// const n = 2;
	// const nFactorial = 2;

	// const n = 3;
	// const nFactorial = 6;

	// const n = 4;
	// const nFactorial = 24;

	const n = 5;
	const nFactorial = 120;

	expect(strCombinatorY).toBeDefined();

	if (typeof strCombinatorY === 'undefined') {
		throw new Error('strCombinatorY is undefined');
	}

	const strFactorial1 = `((${strCombinatorY} ${strG}) ${n})`;

	const actualResult1 = f(strFactorial1);

	// console.log(`Y combinator pre-test 1: actualResult1 is: ${actualResult1};`, actualResult1);

	// actualResult1 is:
	//	(
	//		(
	//			λg.
	//				(
	//					λx.(g (x x))
	//					λx.(g (x x))
	//				)
	//				λr.λn.(([= n 0]  1) [* n (r [- n 1] )] )
	//		)
	//		1
	//	)

	expect(actualResult1).toBeTruthy();

	// const actualResult2 = actualResult1.betaReduce(
	// 	BetaReductionStrategy.CallByName,
	// 	variableNameGenerator,
	// 	2
	// ) as LCFunctionCall;
	const actualResult2 = actualResult1.betaReduce({
		strategy: BetaReductionStrategy.CallByName,
		generateNewVariableName: variableNameGenerator
	}) as LCFunctionCall | LCIntegerLiteral;

	// console.log(`Y combinator pre-test 1: actualResult2 is: ${actualResult2};`, actualResult2);

	let actualResult15: ILCExpression;

	if (isLCIntegerLiteral(actualResult2)) {
		actualResult15 = actualResult2;
	} else {
		actualResult15 = yCombinatorPretest1Reducer(actualResult2, variableNameGenerator);
	}

	// ****

	// const actualResult15 = yCombinatorPretest1Reducer(actualResult2, variableNameGenerator);

	// console.log(`Y combinator pre-test 1: actualResult15 is: ${actualResult15};`, actualResult15);

	// actualResult15 === 1

	expect(actualResult15).toBeTruthy();
	expect(isLCIntegerLiteral(actualResult15)).toBeTruthy();
	expect((actualResult15 as LCIntegerLiteral).value).toBe(nFactorial);

	// Thus 1! === 1
});

test('LambdaCalculusIntegerExtensionGrammar Y combinator pre-test 2', () => {
	const actualResult0 = yCombinatorPretest1ReducerLauncher(0);
	const actualResult1 = yCombinatorPretest1ReducerLauncher(1);
	const actualResult2 = yCombinatorPretest1ReducerLauncher(2);
	const actualResult3 = yCombinatorPretest1ReducerLauncher(3);
	const actualResult4 = yCombinatorPretest1ReducerLauncher(4);
	const actualResult5 = yCombinatorPretest1ReducerLauncher(5);

	expect(actualResult0).toBeTruthy();
	expect(isLCIntegerLiteral(actualResult0)).toBeTruthy();
	expect((actualResult0 as LCIntegerLiteral).value).toBe(1);

	expect(actualResult1).toBeTruthy();
	expect(isLCIntegerLiteral(actualResult1)).toBeTruthy();
	expect((actualResult1 as LCIntegerLiteral).value).toBe(1);

	expect(actualResult2).toBeTruthy();
	expect(isLCIntegerLiteral(actualResult2)).toBeTruthy();
	expect((actualResult2 as LCIntegerLiteral).value).toBe(2);

	expect(actualResult3).toBeTruthy();
	expect(isLCIntegerLiteral(actualResult3)).toBeTruthy();
	expect((actualResult3 as LCIntegerLiteral).value).toBe(6);

	expect(actualResult4).toBeTruthy();
	expect(isLCIntegerLiteral(actualResult4)).toBeTruthy();
	expect((actualResult4 as LCIntegerLiteral).value).toBe(24);

	expect(actualResult5).toBeTruthy();
	expect(isLCIntegerLiteral(actualResult5)).toBeTruthy();
	expect((actualResult5 as LCIntegerLiteral).value).toBe(120);
});

// test('LambdaCalculusIntegerExtensionGrammar Church numerals x 10 test', () => {
// 	// const limit = 3;
// 	const limit = 10;
// 	const grammar = createGrammar(ls);
// 	const tokenizer = createTokenizer(LexicalAnalyzerSelector.MidnightHack, ls);
// 	const parser = createParser(ParserSelector.LL1, grammar);
// 	const fb = getfb2(tokenizer, parser);
// 	// , {
// 	// 	generateNewVariableName: createVariableNameGenerator(),
// 	// 	// maxDepth: limit + 3
// 	// 	maxDepth: 100
// 	// });
//
// 	for (let i = 0; i < limit; i++) {
// 		const expectedResult = i;
// 		const iChurchExpr = mapLCExprNamesToStrings.get(`${i}`);
// 		const iChurchExprAsInt = `((${iChurchExpr} λn.(+ n 1)) 0)`;
// 		const actualResult = fb(iChurchExprAsInt);
//
// 		// console.log(`${i}: ${iChurchExpr} -> ${actualResult}`);
//
// 		expect(actualResult).toBeTruthy();
// 		expect(isLCIntegerLiteral(actualResult)).toBeTruthy();
// 		expect((actualResult as LCIntegerLiteral).value).toBe(expectedResult);
// 	}
// });
