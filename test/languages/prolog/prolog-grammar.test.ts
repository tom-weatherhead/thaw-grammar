// tom-weatherhead/thaw-grammar/test/languages/prolog/prolog-grammar.test.ts

'use strict';

import {
	// IEqualityComparable,
	isIEqualityComparable,
	Set
} from 'thaw-common-utilities.ts';

import {
	createGrammar,
	LanguageSelector,
	PrologGlobalInfo,
	PrologVariable
} from '../../..';

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
	const setWith = new Set<PrologVariable>();
	const setWithout = new Set<PrologVariable>();
	const variable = new PrologVariable('A');

	setWith.add(new PrologVariable('B'));
	setWith.add(new PrologVariable('A'));
	setWith.add(new PrologVariable('C'));

	setWithout.add(new PrologVariable('B'));
	setWithout.add(new PrologVariable('C'));

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
