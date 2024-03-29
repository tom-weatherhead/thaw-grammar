// tom-weatherhead/thaw-grammar/test/languages/prolog/prolog-grammar.test.ts

'use strict';

import {
	createSet,
	// IEqualityComparable,
	isIEqualityComparable
} from 'thaw-common-utilities.ts';

import { GrammarSymbol, LanguageSelector } from 'thaw-interpreter-types';

import { createProduction } from 'thaw-interpreter-core';

import {
	createGrammar,
	createPrologVariable,
	// deepEquals,
	IPrologExpression,
	isIPrologVariable,
	// isProduction,
	IPrologVariable,
	// createProduction,
	// PrologFunctor,
	PrologFunctorExpression,
	PrologGlobalInfo,
	PrologGoal,
	PrologIntegerLiteral
	// PrologPredicate,
	// PrologVariable,
} from '../../..';

// test('deepEquals test', () => {
// 	// Arrange
// 	// const grammar = createGrammar(LanguageSelector.Prolog);
// 	// const grammar = createGrammar(LanguageSelector.Prolog2);

// 	// Act
// 	// Assert

// 	// expect(deepEquals(, )).toBeTruthy();
// 	// expect(deepEquals(, )).toBeFalsy();

// 	expect(deepEquals(0, 0)).toBeTruthy();
// 	expect(deepEquals(1, 1)).toBeTruthy();
// 	expect(deepEquals(0, 1)).toBeFalsy();

// 	expect(
// 		deepEquals(new Date('2021-01-01'), new Date('2021-01-01'))
// 	).toBeTruthy();

// 	expect(deepEquals({}, {})).toBeTruthy();
// 	expect(deepEquals([], [])).toBeTruthy();
// 	expect(deepEquals({}, [])).toBeFalsy();

// 	expect(deepEquals([1, 2, [3, 4, 5]], [1, 2, [3, 4, 5]])).toBeTruthy();
// 	expect(deepEquals([1, 2, [3, 4, 5]], [1, 2, [4, 4, 5]])).toBeFalsy();
// 	expect(deepEquals([1, 2, [3, 4, 5]], [1, 2, [4, 4, 5], 6])).toBeFalsy();

// 	expect(
// 		deepEquals([{ a: 1 }, { b: 2, c: 3 }], [{ a: 1 }, { b: 2, c: 3 }])
// 	).toBeTruthy();
// 	expect(
// 		deepEquals([{ a: 1 }, { b: 2, c: 3 }], [{ a: 1 }, { c: 3, b: 2 }])
// 	).toBeTruthy();
// 	expect(
// 		deepEquals([{ a: 1 }, { b: 2, c: 3 }], [{ b: 2, c: 3 }, { a: 1 }])
// 	).toBeFalsy();
// });

test('PrologGrammar instance creation test', () => {
	// Arrange
	// const grammar = createGrammar(LanguageSelector.Prolog);
	const grammar = createGrammar(LanguageSelector.Prolog2);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});

test('PrologGlobalInfo instance creation test', () => {
	// Arrange
	// const grammar = createGrammar(LanguageSelector.Prolog);
	// const grammar = createGrammar(LanguageSelector.Prolog2);
	const globalInfo = new PrologGlobalInfo();

	// Act
	// Assert
	expect(globalInfo).toBeTruthy();
});

// test('Find PrologVariable in array test', () => {
// 	// Arrange
// 	// const grammar = createGrammar(LanguageSelector.Prolog);
// 	// const grammar = createGrammar(LanguageSelector.Prolog2);
// 	// const globalInfo = new PrologGlobalInfo();
// 	const variable = new PrologVariable('A');
// 	const arrayWith = [
// 		new PrologVariable('B'),
// 		new PrologVariable('A'),
// 		new PrologVariable('C')
// 	];
// 	const arrayWithout = [new PrologVariable('B'), new PrologVariable('C')];

// 	// Act
// 	// Assert
// 	expect(arrayWith.includes(variable)).toBeTruthy();
// 	expect(arrayWithout.includes(variable)).toBeFalsy();

// 	expect(variable in arrayWith).toBeTruthy();
// 	expect(variable in arrayWithout).toBeFalsy();

// 	expect(arrayWith.indexOf(variable) >= 0).toBeTruthy();
// 	expect(arrayWithout.indexOf(variable) >= 0).toBeFalsy();
// });

test('Find PrologVariable in Set test', () => {
	// Arrange
	// const grammar = createGrammar(LanguageSelector.Prolog);
	// const grammar = createGrammar(LanguageSelector.Prolog2);
	// const globalInfo = new PrologGlobalInfo();
	const setWith = createSet<IPrologVariable>();
	const setWithout = createSet<IPrologVariable>();
	const variable = createPrologVariable('A');

	setWith.add(createPrologVariable('B'));
	setWith.add(createPrologVariable('A'));
	setWith.add(createPrologVariable('C'));

	setWithout.add(createPrologVariable('B'));
	setWithout.add(createPrologVariable('C'));

	// Act
	// Assert
	// const aaa = variable as IEqualityComparable;
	const globalInfo = new PrologGlobalInfo();
	// const bbb = globalInfo as IEqualityComparable;

	// console.log('variable as IEqualityComparable is:', typeof aaa, aaa);
	// console.log('globalInfo as IEqualityComparable is:', typeof bbb, bbb);

	expect(isIEqualityComparable(variable)).toBeTruthy();
	expect(isIEqualityComparable(globalInfo)).toBeFalsy();

	expect(setWith.contains(variable)).toBeTruthy();
	expect(setWithout.contains(variable)).toBeFalsy();
});

test('PrologGlobalInfo instanceof test', () => {
	// Arrange
	const ls = LanguageSelector.Prolog2;
	const intlit = new PrologIntegerLiteral(13);
	const variable = createPrologVariable('A');
	const exprList: IPrologExpression[] = [];
	// const predicate = new PrologPredicate('pred');
	const goal = new PrologGoal(ls, 'pred', exprList);
	// const functor = new PrologFunctor('functor');
	const functorExpression = new PrologFunctorExpression(ls, 'functor', exprList);

	// Act
	// Assert
	expect(functorExpression instanceof PrologFunctorExpression).toBeTruthy();
	expect(functorExpression instanceof PrologGoal).toBeFalsy();
	expect(functorExpression instanceof PrologIntegerLiteral).toBeFalsy();
	expect(isIPrologVariable(functorExpression)).toBeFalsy();

	expect(goal instanceof PrologFunctorExpression).toBeFalsy();
	expect(goal instanceof PrologGoal).toBeTruthy();
	expect(goal instanceof PrologIntegerLiteral).toBeFalsy();
	expect(isIPrologVariable(goal)).toBeFalsy();

	expect(intlit instanceof PrologFunctorExpression).toBeFalsy();
	expect(intlit instanceof PrologGoal).toBeFalsy();
	expect(intlit instanceof PrologIntegerLiteral).toBeTruthy();
	expect(isIPrologVariable(intlit)).toBeFalsy();

	expect(variable instanceof PrologFunctorExpression).toBeFalsy();
	expect(variable instanceof PrologGoal).toBeFalsy();
	expect(variable instanceof PrologIntegerLiteral).toBeFalsy();
	expect(isIPrologVariable(variable)).toBeTruthy();
});

test('Prolog type guard test', () => {
	const p = createProduction(
		GrammarSymbol.nonterminalStart,
		[GrammarSymbol.nonterminalExpression, GrammarSymbol.terminalEOF, '#action'],
		1337
	);

	// console.log('Prolog type guard test: typeof p.lhs is:', typeof p.lhs); // number
	// console.log('Prolog type guard test: typeof p.rhs is:', typeof p.rhs); // object
	// console.log('Prolog type guard test: p.rhs instanceof Array is:', p.rhs instanceof Array); // true
	// console.log('Prolog type guard test: typeof p.rhs.length is:', typeof p.rhs.length);
	// console.log('Prolog type guard test: typeof p.rhs[0] is:', typeof p.rhs[0]); // number
	// console.log('Prolog type guard test: typeof p.rhs[1] is:', typeof p.rhs[1]); // number
	// console.log('Prolog type guard test: typeof p.rhs[2] is:', typeof p.rhs[2]); // string
	// console.log('Prolog type guard test: typeof p.num is:', typeof p.num); // number

	expect(p).toBeTruthy();
	// expect(isProduction(p)).toBeTruthy();
	expect(p.equals(p)).toBeTruthy();
});
