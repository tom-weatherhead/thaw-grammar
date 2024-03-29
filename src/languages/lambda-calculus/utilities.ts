// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/utilities.ts

import { ifDefinedThenElse } from 'thaw-common-utilities.ts';

import { IParser, ITokenizer } from 'thaw-interpreter-types';

import {
	BetaReductionStrategy,
	ILCExpression,
	IUnifiable
} from './domain-object-model/interfaces/expression';

// export const defaultBetaReductionStrategy = BetaReductionStrategy.CallByName;
export const defaultBetaReductionStrategy = BetaReductionStrategy.NormalOrder;

// export const defaultMaxBetaReductionDepth = 20; // 2021-09-28 19:01 with CallByName : 2 tests fail.
export const defaultMaxBetaReductionDepth = 30;
// export const defaultMaxBetaReductionDepth = 50;
// export const defaultMaxBetaReductionDepth = 100;

// const strTrue = 'λx.λy.x';
// const strFalse = 'λx.λy.y';
//
// const strIf = 'λb.λx.λy.((b x) y)'; // if b then x else y
//
// export const mapLCExprNamesToStrings = new Map<string, string>();
//
// mapLCExprNamesToStrings.set('identity', 'λx.x');
//
// mapLCExprNamesToStrings.set('true', strTrue);
// mapLCExprNamesToStrings.set('false', strFalse);
//
// mapLCExprNamesToStrings.set('0', 'λf.λx.x');
// mapLCExprNamesToStrings.set('1', 'λf.λx.(f x)');
// mapLCExprNamesToStrings.set('2', 'λf.λx.(f (f x))');
// mapLCExprNamesToStrings.set('3', 'λf.λx.(f (f (f x)))');
// mapLCExprNamesToStrings.set('4', 'λf.λx.(f (f (f (f x))))');
// mapLCExprNamesToStrings.set('5', 'λf.λx.(f (f (f (f (f x)))))');
// mapLCExprNamesToStrings.set('6', 'λf.λx.(f (f (f (f (f (f x))))))');
// mapLCExprNamesToStrings.set('7', 'λf.λx.(f (f (f (f (f (f (f x)))))))');
// mapLCExprNamesToStrings.set('8', 'λf.λx.(f (f (f (f (f (f (f (f x))))))))');
// mapLCExprNamesToStrings.set('9', 'λf.λx.(f (f (f (f (f (f (f (f (f x)))))))))');
//
// mapLCExprNamesToStrings.set('isZero', `λn.((n λx.${strFalse}) ${strTrue})`);
// mapLCExprNamesToStrings.set('successor', 'λn.λf.λx.(f ((n f) x))');
// mapLCExprNamesToStrings.set('predecessor', 'λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)');
// mapLCExprNamesToStrings.set('if', strIf);
// mapLCExprNamesToStrings.set('and', `λp.λq.((p q) ${strFalse})`);
// mapLCExprNamesToStrings.set('or', `λp.λq.(((${strIf} p) ${strTrue}) q)`);
// mapLCExprNamesToStrings.set('add', 'λm.λn.λf.λx.((n f) ((m f) x))');
// mapLCExprNamesToStrings.set('multiply', 'λm.λn.λf.(m (n f))');
// // mapLCExprNamesToStrings.set('', '');
//
// export const mapCombinatorNamesToStrings = new Map<string, string>();
//
// mapCombinatorNamesToStrings.set('I', 'λx.x');
// mapCombinatorNamesToStrings.set('K', 'λx.λy.x'); // === strTrue
// mapCombinatorNamesToStrings.set('S', 'λa.λb.λc.((a c) (b c))');
// // Y is a fixed-point combinator; it is used to implement recursion.
// mapCombinatorNamesToStrings.set('Y', 'λa.(λb.(a (b b)) λb.(a (b b)))');
// // mapCombinatorNamesToStrings.set('', '');

export function getParseFunction(
	tokenizer: ITokenizer,
	parser: IParser
): (str: string) => ILCExpression {
	return (str: string) => parser.parse(tokenizer.tokenize(str)) as ILCExpression;
}

export function createVariableNameGenerator(): () => string {
	let n = 0;

	return () => `v${++n}`;
}

export function reduce(
	expr: ILCExpression,
	options: {
		readonly strategy?: BetaReductionStrategy;
		readonly generateNewVariableName?: () => string;
		readonly maxDepth?: number;
	} = {}
): ILCExpression {
	const generateNewVariableName = createVariableNameGenerator();

	return expr.betaReduce({
		strategy: options.strategy,
		generateNewVariableName: ifDefinedThenElse(
			options.generateNewVariableName,
			generateNewVariableName
		),
		maxDepth: options.maxDepth
	});
}

export function getfb1(
	f: (str: string) => ILCExpression,
	options: {
		readonly strategy?: BetaReductionStrategy;
		readonly generateNewVariableName?: () => string;
		readonly maxDepth?: number;
	} = {}
): (s: string) => ILCExpression {
	// const generateNewVariableName = createVariableNameGenerator();
	//
	// return (s: string): ILCExpression =>
	// 	f(s).betaReduce({
	// 		strategy: options.strategy,
	// 		generateNewVariableName: ifDefinedThenElse(
	// 			options.generateNewVariableName,
	// 			generateNewVariableName
	// 		),
	// 		maxDepth: options.maxDepth
	// 	});

	return (s: string): ILCExpression => reduce(f(s), options);
}

export function getfb2(
	tokenizer: ITokenizer,
	parser: IParser,
	options: {
		readonly strategy?: BetaReductionStrategy;
		readonly generateNewVariableName?: () => string;
		readonly maxDepth?: number;
	} = {}
): (s: string) => ILCExpression {
	return getfb1(getParseFunction(tokenizer, parser), options);
}

// export function createMapOfLCExprNamesToExprs(
// 	fb: (s: string) => ILCExpression
// ): Map<string, ILCExpression> {
// 	const mapLCExprNamesToExprs = new Map<string, ILCExpression>();
//
// 	for (const [key, value] of Object.entries(mapLCExprNamesToStrings)) {
// 		mapLCExprNamesToExprs.set(key, fb(value));
// 	}
//
// 	return mapLCExprNamesToExprs;
// }

export function areIsomorphic<T>(expr1: IUnifiable<T>, expr2: IUnifiable<T>): boolean {
	const unifyingSubstitution = expr1.unify(expr2);

	return typeof unifyingSubstitution !== 'undefined' && unifyingSubstitution.isOneToOne;
}
