//

'use strict';

import { LanguageSelector, LexicalAnalyzerSelector, ParserSelector } from 'thaw-interpreter-types';

import { createTokenizer } from 'thaw-lexical-analyzer';

import {
	BetaReductionStrategy,
	createGrammar,
	ILCExpression,
	isLCIntegerLiteral,
	LCFunctionCall,
	LCIntegerLiteral,
	LCLambdaExpression,
	LCPrimitiveOperator
} from '../../..';

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

test('LambdaCalculusIntegerExtensionGrammar integer subtraction test', () => {
	const f = getParseFunction();
	const three = f('[- 8 5]').deltaReduce();

	expect(`${three}`).toBe('3');
	expect(isLCIntegerLiteral(three)).toBeTruthy();
	expect((three as LCIntegerLiteral).value).toBe(3);
});

test('LambdaCalculusIntegerExtensionGrammar integer multiplication test', () => {
	const f = getParseFunction();
	const ninetyOne = f('[* 7 13]').deltaReduce();

	expect(`${ninetyOne}`).toBe('91');
	expect(isLCIntegerLiteral(ninetyOne)).toBeTruthy();
	expect((ninetyOne as LCIntegerLiteral).value).toBe(91);
});

test('LambdaCalculusIntegerExtensionGrammar if test', () => {
	const f = getParseFunction();
	const variableNameGenerator = createVariableNameGenerator();

	const actualResult1 = f('if [= 0 0] 7 13')
		.deltaReduce()
		.betaReduce(BetaReductionStrategy.CallByName, variableNameGenerator, 10);
	const actualResult2 = f('if [= 0 1] 7 13')
		.deltaReduce()
		.betaReduce(BetaReductionStrategy.CallByName, variableNameGenerator, 10);

	expect(isLCIntegerLiteral(actualResult1)).toBeTruthy();
	expect(isLCIntegerLiteral(actualResult2)).toBeTruthy();
	expect((actualResult1 as LCIntegerLiteral).value).toBe(7);
	expect((actualResult2 as LCIntegerLiteral).value).toBe(13);
});

test('LambdaCalculusIntegerExtensionGrammar Y combinator pre-test 1', () => {
	const f = getParseFunction();
	const variableNameGenerator = createVariableNameGenerator();

	const strG = 'λr.λn.if [= n 0] 1 [* n (r [- n 1])]';

	expect(f(strG)).toBeTruthy();

	const strFactorial1 = `((${strCombinatorY} ${strG}) 1)`;

	const actualResult1 = f(strFactorial1);

	console.log(`Y combinator pre-test 1: actualResult1 is: ${actualResult1};`, actualResult1);

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

	const actualResult2 = actualResult1.betaReduce(
		BetaReductionStrategy.CallByName,
		variableNameGenerator,
		2
	) as LCFunctionCall;

	console.log(`Y combinator pre-test 1: actualResult2 is: ${actualResult2};`, actualResult2);

	// LCFunctionCall.betaReduceCore() : lambdaExpressionBody is (λx.(g (x x)) λx.(g (x x)))

	// LCFunctionCall.betaReduceCore() : Replaced g with:
	// λr.λn.(([= n 0]  1) [* n (r [- n 1] )] );
	// bodyAfterSubst is (λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x)) λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x)))

	// actualResult2 is: ((λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x)) λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x))) 1)

	expect(actualResult2).toBeTruthy();

	const f3 = (actualResult2 as LCFunctionCall).callee as LCFunctionCall;

	console.log(`Y combinator pre-test 1: f3 is: ${f3};`, f3);

	// f3 is: (λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1] )] ) (x x)) λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)))

	// const actualResult3 = f3.betaReduce(BetaReductionStrategy.CallByName, variableNameGenerator, 1);
	//
	// console.log(`Y combinator pre-test 1: actualResult3 is: ${actualResult3};`, actualResult3);

	// actualResult3 is: (λv2.λv3.(([= v3 0]  1) [* v3 (v2 [- v3 1] )] ) (λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x)) λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x))))

	const actualResult3a = f3.betaReduceCore(
		f3.callee as LCLambdaExpression,
		f3.arg,
		variableNameGenerator
	) as LCFunctionCall;

	console.log(`Y combinator pre-test 1: actualResult3a is: ${actualResult3a};`, actualResult3a);

	// actualResult3a is: (λv2.λv3.(([= v3 0]  1) [* v3 (v2 [- v3 1] )] ) (λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x)) λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x))))

	// lambdaExpressionBody is (λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x))

	// Replaced x with λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x))

	// actualResult3a is: (λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x)) λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x))))

	const actualResult4 = actualResult3a.betaReduceCore(
		actualResult3a.callee as LCLambdaExpression,
		actualResult3a.arg,
		variableNameGenerator
	) as LCLambdaExpression;

	console.log(`Y combinator pre-test 1: actualResult4 is: ${actualResult4};`, actualResult4);
	console.log(
		`Y combinator pre-test 1: actualResult2.arg is: ${actualResult2.arg};`,
		actualResult2.arg
	);

	// actualResult4 is: λn.(([= n 0] 1) [* n ((λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)) λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x))) [- n 1])])

	const actualResult5 = actualResult4.body.substituteForUnboundVariable(
		actualResult4.arg.name,
		actualResult2.arg
	);

	console.log(`Y combinator pre-test 1: actualResult5 is: ${actualResult5};`, actualResult5);

	// actualResult5 is: (([= 1 0] 1) [* 1 ((λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)) λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x))) [- 1 1])])

	const actualResult6 = actualResult5.deltaReduce() as LCFunctionCall;

	console.log(`Y combinator pre-test 1: actualResult6 is: ${actualResult6};`, actualResult6);

	// actualResult6 is: ((λx.λy.y 1) [* 1 ((λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)) λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x))) 0)])

	const actualResult7 = actualResult6.callee.betaReduce(
		BetaReductionStrategy.CallByName,
		variableNameGenerator,
		10
	);

	console.log(`Y combinator pre-test 1: actualResult7 is: ${actualResult7};`, actualResult7);

	// actualResult7 is: λy.y

	const actualResult8 = actualResult6.betaReduceCore(
		actualResult7 as LCLambdaExpression,
		actualResult6.arg,
		variableNameGenerator
	) as LCPrimitiveOperator;

	console.log(`Y combinator pre-test 1: actualResult8 is: ${actualResult8};`, actualResult8);

	// actualResult8 is: [* 1 ((λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)) λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x))) 0)]

	// vs. actualResult2 is: ((λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x)) λx.(λr.λn.(([= n 0]  1) [* n (r [- n 1] )] ) (x x))) 1)

	// Compare f9 vs. f3
	const f9 = (actualResult8.rightChild as LCFunctionCall).callee as LCFunctionCall;

	console.log(`Y combinator pre-test 1: f9 is: ${f9};`, f9);

	// f9 is: (λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)) λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)))

	const actualResult9 = f9.betaReduceCore(
		f9.callee as LCLambdaExpression,
		f9.arg,
		variableNameGenerator
	) as LCFunctionCall;

	console.log(`Y combinator pre-test 1: actualResult9 is: ${actualResult9};`, actualResult9);

	// actualResult9 is: (λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)) λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x))))

	const actualResult10 = actualResult9.betaReduceCore(
		actualResult9.callee as LCLambdaExpression,
		actualResult9.arg,
		variableNameGenerator
	) as LCLambdaExpression;

	console.log(`Y combinator pre-test 1: actualResult10 is: ${actualResult10};`, actualResult10);

	// actualResult10 is: λn.(([= n 0] 1) [* n ((λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)) λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x))) [- n 1])])

	const actualResult11 = actualResult10.body.substituteForUnboundVariable(
		actualResult10.arg.name,
		(actualResult8.rightChild as LCFunctionCall).arg
	);

	console.log(`Y combinator pre-test 1: actualResult11 is: ${actualResult11};`, actualResult11);

	// actualResult11 is: (([= 0 0] 1) [* 0 ((λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)) λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x))) [- 0 1])])

	const actualResult12 = actualResult11.deltaReduce() as LCFunctionCall;

	console.log(`Y combinator pre-test 1: actualResult12 is: ${actualResult12};`, actualResult12);

	// actualResult12 is: ((λx.λy.x 1) [* 0 ((λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x)) λx.(λr.λn.(([= n 0] 1) [* n (r [- n 1])]) (x x))) -1)])
	// I.e. if TRUE then 1 else ...

	const actualResult13 = actualResult12.callee.betaReduce(
		BetaReductionStrategy.CallByName,
		variableNameGenerator,
		10
	);

	console.log(`Y combinator pre-test 1: actualResult13 is: ${actualResult13};`, actualResult13);

	// actualResult13 is: λy.1

	const actualResult14 = actualResult12.betaReduceCore(
		actualResult13 as LCLambdaExpression,
		actualResult12.arg,
		variableNameGenerator
	);

	console.log(`Y combinator pre-test 1: actualResult14 is: ${actualResult14};`, actualResult14);

	// actualResult8.rightChild === actualResult14 === 1

	expect(actualResult14).toBeTruthy();
	expect(isLCIntegerLiteral(actualResult14)).toBeTruthy();
	expect((actualResult14 as LCIntegerLiteral).value).toBe(1);

	const f15 = new LCPrimitiveOperator(
		actualResult8.name,
		actualResult8.leftChild,
		actualResult14
	);
	const actualResult15 = f15.deltaReduce();

	console.log(`Y combinator pre-test 1: actualResult15 is: ${actualResult15};`, actualResult15);

	expect(actualResult15).toBeTruthy();
	expect(isLCIntegerLiteral(actualResult15)).toBeTruthy();
	expect((actualResult15 as LCIntegerLiteral).value).toBe(1);

	// Thus 1! === 1
});

// test('LambdaCalculusIntegerExtensionGrammar Y combinator full test', () => {
// 	const f = getParseFunction();
// 	const variableNameGenerator = createVariableNameGenerator();
//
// 	const strG = 'λr.λn.if [= n 0] 1 [* n (r [- n 1])]';
//
// 	expect(f(strG)).toBeTruthy();
//
// 	const strFactorial0 = `((${strCombinatorY} ${strG}) 0)`;
// 	const strFactorial1 = `((${strCombinatorY} ${strG}) 1)`;
// 	const strFactorial5 = `((${strCombinatorY} ${strG}) 5)`;
//
// 	const actualResult0 = f(strFactorial0).betaReduce(
// 		BetaReductionStrategy.CallByName,
// 		variableNameGenerator,
// 		10
// 	);
// 	const actualResult1 = f(strFactorial1).betaReduce(
// 		BetaReductionStrategy.CallByName,
// 		variableNameGenerator,
// 		10
// 	);
// 	const actualResult5 = f(strFactorial5).betaReduce(
// 		BetaReductionStrategy.CallByName,
// 		variableNameGenerator,
// 		10
// 	);
//
// 	console.log(
// 		`LambdaCalculusIntegerExtensionGrammar Y combinator test : actualResult0 is ${actualResult0};`,
// 		actualResult0
// 	);
//
// 	console.log(
// 		`LambdaCalculusIntegerExtensionGrammar Y combinator test : actualResult1 is ${actualResult1};`,
// 		actualResult1
// 	);
//
// 	console.log(
// 		`LambdaCalculusIntegerExtensionGrammar Y combinator test : actualResult5 is ${actualResult5};`,
// 		actualResult5
// 	);
//
// 	expect(actualResult0).toBeTruthy();
// 	expect(isLCIntegerLiteral(actualResult0)).toBeTruthy();
// 	expect((actualResult0 as LCIntegerLiteral).value).toBe(1);
//
// 	expect(actualResult1).toBeTruthy();
// 	// expect(isLCIntegerLiteral(actualResult1)).toBeTruthy();
// 	// expect((actualResult1 as LCIntegerLiteral).value).toBe(1);
//
// 	expect(actualResult5).toBeTruthy();
// 	// expect(isLCIntegerLiteral(actualResult5)).toBeTruthy();
// 	// expect((actualResult5 as LCIntegerLiteral).value).toBe(120);
// });
