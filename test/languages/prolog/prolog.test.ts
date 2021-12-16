// tom-weatherhead/thaw-parser/test/prolog.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import {
	createFnParser,
	createFnRecognizer,
	createInfrastructure
} from '../../create-infrastructure';

import {
	createGrammar,
	createVariable,
	PrologClause,
	PrologGlobalInfo,
	PrologGoal
} from '../../..';

import { createParser, SyntaxException } from 'thaw-parser';

const ls = LanguageSelector.Prolog2;

test('LL(1) Prolog parser instance creation test', () => {
	// Arrange
	const grammar = createGrammar(ls);

	// Act
	const parser = createParser(grammar.defaultParser, grammar);

	// Assert
	expect(parser).toBeTruthy();
});

test('LL(1) Prolog recognize test', () => {
	// Arrange
	const f = createFnRecognizer(ls);

	f('pred1.');
	f('pred1(A).');
	f('pred1(A, B, C).');
	f('pred1(A) :- pred2(A).');

	f('accRev(cons(H, T), A, R):-  accRev(T, cons(H, A), R).');
	f('accRev(nil, A, A).');
	f('?- accRev(cons(1, cons(2, nil)), nil, R).');

	f('pred1([]).');
	f('pred1([1]).');
	f('pred1([1, 2]).');
	f('pred1([1, 2, 3]).');
	f('pred1([1 | [2, 3]]).');

	f('unique_list([X|L]) :- \\+ member(X, L), unique_list(L).');

	f('factorial(0, 1).');

	f('factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.');

	f('factorial(N, F) :- gt(N, 0), sub(N, 1, N1), factorial(N1, F1), mult(N, F1, F).');

	// f('G(X) :- H(X), !, I(X).');
	f('g(X) :- h(X), !, i(X).');

	expect(() => f('pred1(A.')).toThrow(SyntaxException);

	f('true.');
	f('member(X, cons(Y, M)) :- member(X, M).');
	f('?- member(3, cons(2, cons(3, nil))).');
	f('increment(X, Y) :- Y is X + 1.');
	f('?- 0 < 1.');
	f('foo([]).');
	f('foo([X | L]).');
	f('foo([1, 2, 3]).');
	// f('foo((1)).');
	// f('foo(A, B, C, D) :- D is A * 7 + B / 13 - C.');
	f('foo(X) :- \\+ bar(X).');
	// f('foo(X, Y) :- bar(X, Y) ; bar(Y, X).');	// Goal disjunction
	// f('foo(X) :- (bar(X) -> baz(X) : bat(X)).');
	f('foo(X) :- X == 13.');
	f('foo(X) :- X \\== 13.');
	// f('?- mia(t) = mia(X).');	// PrologGrammar2 (non-LL(1)) yields a reduce-reduce conflict for the SLR(1) and LALR(1) parsers.
	f('?- 1 = X.');
	// f('?- X = mia.');
	// f('?- X = mia(t).');
	f('?- 1 \\= X.');
	// f('?- X \\= mia.');
	// f('?- X \\= mia(t).');

	// f('?- 2 + 3 < 5 + 7.');
	// f('?- X = 2, Y = 5, X + 3 < Y + 7.');
	// f('?- X is (((1 + 1) + 1) + 1) + 1.');
	// f('?- 5 is (((1 + 1) + 1) + 1) + 1.');
	// f('?- 3 is mod(7, 3) + mod(8, 3).');
	// f('?- (((1 + 1) + 1) + 1) + 1 < 7.');
	// f('?- mod(7, 3) + 1 < mod(8, 3) + 1.');

	// Definite Clause Grammar support.
	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch7
	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlch8
	// f('vp --> v, np.');
	// f('vp --> v.');
	// f('v --> [loves].');
	// f('v --> [a, b, c].');
	// f('s --> ablock(Count), bblock(Count), cblock(Count).');
	// f('ablock(0) --> [].');
	// f('ablock(NewCount) --> [a], ablock(Count), {NewCount is Count + 1}.');

	// bagof()
	// f('?- bagof(Child, Mother ^ descend(Mother, Child), Results).');
	// f('?- bagof(Child, Child ^ Mother ^ descend(Mother, Child), Results).');
	// f('?- bagof(Child, Mother ^ Descend(Mother, Child), Results).');
	// f('?- bagof(Child, Child ^ Mother ^ Descend(Mother, Child), Results).');

	// A clause with a RHS is enclosed in an extra set of brackets.
	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse48
	// f('?- assert(happy(mia)).');
	// f('?- assert((naive(X) :- happy(X))).');
	// f('?- assert((X :- Y, Z(W))).');
	// f('?- assert((X(W) :- Y, Z(W))).');

	// findall()
	// f('?- findall(X, retract((fact(Y) :- foo(X, Y))), Results).');
});

function success(substitutionAsString = ''): string {
	const satisfying =
		substitutionAsString !== ''
			? `Satisfying substitution is: [${substitutionAsString}]\n`
			: '';

	return `${satisfying}${PrologGlobalInfo.Satisfied}\n`;
}

function failure(): string {
	return `${PrologGlobalInfo.NotSatisfied}\n`;
}

function prologTest(
	data: Array<[input: string, expectedResult: string | string[]]>,
	options: { findAllSolutions?: boolean } = {}
): void {
	// Arrange
	const { tokenizer, parser } = createInfrastructure(ls);
	const prologGlobalInfo = new PrologGlobalInfo();

	if (options.findAllSolutions) {
		prologGlobalInfo.FindAllSolutions();
	}

	for (const [input, expectedResult] of data) {
		// Act
		const actualResult = prologGlobalInfo.ProcessInput(
			parser.parse(tokenizer.tokenize(input)) as PrologClause | PrologGoal[]
		);

		// console.log(`input: ${input}\nactualResult:\n${actualResult}\n\n`);

		// Assert
		if (typeof expectedResult === 'string') {
			expect(actualResult).toBe(expectedResult);
		} else {
			for (const str of expectedResult) {
				if (!actualResult.includes(str)) {
					console.error('prologTest() : Expected substring not found.');
					console.error('prologTest() : input:', input);
					console.error('prologTest() : actualResult:', actualResult);
					console.error('prologTest() : str:', str);
					console.error('prologTest() : expectedResult:', expectedResult);
				}

				expect(actualResult.includes(str)).toBe(true);
			}
		}
	}
}

test('LL(1) Prolog arithmetic comparison test 1', () => {
	prologTest([['?- lt(7, 13).', success()]]);
});

test('LL(1) Prolog arithmetic comparison test 2', () => {
	prologTest([['?- lt(17, 13).', failure()]]);
});

test('LL(1) Prolog math test 1 : addition', () => {
	prologTest([['?- add(2, 3, N).', success('N -> 5')]]);
});

test('LL(1) Prolog math test 2 : subtraction', () => {
	prologTest([['?- sub(8, 5, N).', success('N -> 3')]]);
});

test('LL(1) Prolog math test 3 : multiplication', () => {
	prologTest([['?- mult(7, 13, N).', success('N -> 91')]]);
});

test('LL(1) Prolog addition test', () => {
	prologTest([
		['?- add(1, 2, 3).', success()],
		['?- add(1, 1, 3).', failure()],
		['?- add(N, 3, 5).', success('N -> 2')],
		['?- add(2, N, 5).', success('N -> 3')],
		['?- add(2, 3, N).', success('N -> 5')],

		// Use real Prolog syntax:
		['?- 3 is 1 + 2.', success()],
		['?- N is 1 + 2.', success('N -> 3')]
	]);
});

test('LL(1) Prolog subtraction test', () => {
	prologTest([
		['?- sub(8, 5, 3).', success()],
		['?- sub(8, 5, 77).', failure()],
		['?- sub(N, 3, 5).', success('N -> 8')],
		['?- sub(8, N, 3).', success('N -> 5')],
		['?- sub(8, 5, N).', success('N -> 3')],

		['?- 3 is 8 - 5.', success()],
		['?- N is 8 - 5.', success('N -> 3')]
	]);
});

test('LL(1) Prolog multiplication test', () => {
	prologTest([
		['?- mult(7, 13, 91).', success()],
		['?- mult(7, 13, 19).', failure()],
		['?- mult(N, 0, 0).', success('N -> 0')],
		['?- mult(N, 0, 91).', failure()],
		['?- mult(N, 13, 91).', success('N -> 7')],
		['?- mult(0, N, 0).', success('N -> 0')],
		['?- mult(0, N, 91).', failure()],
		['?- mult(7, N, 91).', success('N -> 13')],
		['?- mult(7, 13, N).', success('N -> 91')],

		['?- 91 is 7 * 13.', success()],
		['?- N is 7 * 13.', success('N -> 91')]
	]);
});

test('LL(1) Prolog division test', () => {
	prologTest([
		['?- div(91, 13, 7).', success()],
		['?- div(19, 13, 7).', failure()],
		['?- div(N, 13, 7).', success('N -> 91')],
		// ['?- div(91, N, 7).', success('N -> 13')],
		['?- div(91, 13, N).', success('N -> 7')] // ,

		// ['?- 7 is 91 / 13.', success()],
		// ['?- N is 91 / 13.', success('N -> 7')]
	]);
});

test('LL(1) Prolog less than test', () => {
	prologTest([
		['?- 1 < 2.', success()],
		['?- 2 < 2.', failure()],
		['?- 3 < 2.', failure()]
	]);
});

test('LL(1) Prolog greater than test', () => {
	prologTest([
		['?- 1 > 2.', failure()],
		['?- 2 > 2.', failure()],
		['?- 3 > 2.', success()]
	]);
});

test('LL(1) Prolog less than or equal to test', () => {
	prologTest([
		['?- 1 =< 2.', success()],
		['?- 2 =< 2.', success()],
		['?- 3 =< 2.', failure()]
	]);
});

test('LL(1) Prolog greater than or equal to test', () => {
	prologTest([
		['?- 1 >= 2.', failure()],
		['?- 2 >= 2.', success()],
		['?- 3 >= 2.', success()]
	]);
});

test('LL(1) Prolog arithmetic equals test', () => {
	prologTest([
		['?- 1 =:= 2.', failure()],
		['?- 2 =:= 2.', success()],
		['?- 3 =:= 2.', failure()]
	]);
});

test('LL(1) Prolog arithmetic not equal to test', () => {
	prologTest([
		['?- 1 =\\= 2.', success()],
		['?- 2 =\\= 2.', failure()],
		['?- 3 =\\= 2.', success()]
	]);
});

// Equal: '=='

// test('LL(1) Prolog equals test', () => {
// 	prologTest([
// 		[, ]
// 	]);
// });

// Not equal: '\\=='

// test('LL(1) Prolog not equal test', () => {
// 	prologTest([
// 		[, ]
// 	]);
// });

// Unifiable: '='

// test('LL(1) Prolog unifiable test', () => {
// 	prologTest([
// 		[, ]
// 	]);
// });

// Not unifiable: '\\='

// test('LL(1) Prolog not unifiable test', () => {
// 	prologTest([
// 		[, ]
// 	]);
// });

test('LL(1) Prolog list reverse test', () => {
	prologTest([
		['accRev(cons(H, T), A, R):-  accRev(T, cons(H, A), R).', PrologGlobalInfo.ClauseAdded],
		['accRev(nil, A, A).', PrologGlobalInfo.ClauseAdded],
		['?- accRev(nil, nil, R).', ['Satisfied']],
		['?- accRev(cons(1, cons(2, nil)), nil, R).', ['Satisfied']],
		[
			'?- accRev(cons(1, cons(2, cons(3, nil))), nil, R).',
			['Satisfying substitution is: [R -> cons(3, cons(2, cons(1, nil)))]', 'Satisfied']
		],
		[
			'?- accRev(cons(1, cons(2, cons(3, cons(4, nil)))), nil, R).',
			[
				'Satisfying substitution is: [R -> cons(4, cons(3, cons(2, cons(1, nil))))]',
				'Satisfied'
			]
		]
	]);
});

// function explodingCast<T>(value: unknown): T {
// 	const castValue = value as T;

// 	if (castValue.constructor.name !== T.name) {
// 		// if (!(castValue instanceof T)) {
// 		throw new Error(`explodingCast() : ${value} -> ${castValue}`);
// 	}

// 	return castValue;
// }

test('LL(1) Prolog unification test 1', () => {
	// Arrange
	// const ls = LanguageSelector.Prolog2;
	// const prologGlobalInfo = new PrologGlobalInfo();
	// const grammar = createGrammar(ls);
	// const tokenizer = createTokenizer(LexicalAnalyzerSelector.MidnightHack, ls);
	// const parser = createParser(ParserSelector.LL1, grammar);
	//
	// const tpc = <T>(str: string): T =>
	// 	explodingCast<T>(
	// 		prologGlobalInfo.ProcessInput(parser.parse(tokenizer.tokenize(str)))
	// 	);

	const parse = createFnParser<PrologClause>(ls);

	const x = createVariable('X');
	// const clause1 = parse('baz(Z) :- assert((foo(Z) :- bat(Z))).');
	const clause2 = parse('foo(Y) :- not(bar(Y)).');
	const clause3 = parse('foo([1,2,3]).');
	// const clause4 = parse('baz(7) :- assert((foo(7) :- bat(7))).');
	const clause5 = parse('foo(13) :- not(bar(13)).');
	const goal = clause2.Lhs;
	const goalWithInnerGoal = clause2.Rhs[0];
	// const goalWithInnerClause = clause1.Rhs[0];
	const functorExpression = clause3.Lhs.ExpressionList[0];

	expect(x).toBeTruthy();
	expect(clause2).toBeTruthy();
	expect(clause3).toBeTruthy();
	expect(clause5).toBeTruthy();

	expect(x.Unify(functorExpression)).toBeTruthy();
	expect(functorExpression.Unify(x)).toBeTruthy();
	// expect(x.Unify(goal)).toBeFalsy();				// Fails
	expect(goal.Unify(x)).toBeFalsy();
	// expect(x.Unify(goalWithInnerGoal)).toBeFalsy();	// Fails
	expect(goalWithInnerGoal.Unify(x)).toBeFalsy();
	// expect(x.Unify(goalWithInnerClause)).toBeFalsy();
	// expect(goalWithInnerClause.Unify(x)).toBeFalsy();
	// expect(x.Unify(clause3)).toBeFalsy();			// Call stack overflow
	// expect(clause3.Unify(x)).toBeFalsy();
	expect(clause2.Lhs.Unify(clause3.Lhs)).toBeTruthy();
	expect(clause3.Lhs.Unify(clause2.Lhs)).toBeTruthy();
	// expect(clause1.Unify(clause4)).toBeTruthy();
	// expect(clause4.Unify(clause1)).toBeTruthy();
	expect(clause2.Unify(clause5)).toBeTruthy();
	expect(clause5.Unify(clause2)).toBeTruthy();
});

test('LL(1) Prolog list reversal test', () => {
	prologTest([
		['accRev([H | T], A, R):-  accRev(T, [H | A], R).', PrologGlobalInfo.ClauseAdded],
		['accRev([], A, A).', PrologGlobalInfo.ClauseAdded],
		['rev(L, R) :- accRev(L, [], R).', PrologGlobalInfo.ClauseAdded],
		['?- rev([1, 2, 3, 4], R).', success('R -> [4, 3, 2, 1]')],
		['?- accRev([1, 2, 3, 4], [], R).', ['Satisfied']],
		['?- accRev([], [], R).', ['Satisfied', '[R -> []]']],
		['?- accRev([1], [], R).', ['Satisfied', '[R -> [1]]']],
		['?- accRev([1, 2], [], R).', ['Satisfied', '[R -> [2, 1]]']],
		['?- accRev([1, 2, 3], [], R).', ['Satisfied', '[R -> [3, 2, 1]]']],
		['?- accRev([1, 2, 3, 4], [], R).', ['Satisfied', '[R -> [4, 3, 2, 1]]']]
	]);
});

test('LL(1) Prolog Italian crossword test', () => {
	prologTest([
		['word(astante,  a,s,t,a,n,t,e).', PrologGlobalInfo.ClauseAdded],
		['word(astoria,  a,s,t,o,r,i,a).', PrologGlobalInfo.ClauseAdded],
		['word(baratto,  b,a,r,a,t,t,o).', PrologGlobalInfo.ClauseAdded],
		['word(cobalto,  c,o,b,a,l,t,o).', PrologGlobalInfo.ClauseAdded],
		['word(pistola,  p,i,s,t,o,l,a).', PrologGlobalInfo.ClauseAdded],
		['word(statale,  s,t,a,t,a,l,e).', PrologGlobalInfo.ClauseAdded],
		['member(X,[X|_]).', PrologGlobalInfo.ClauseAdded],
		['member(X,[_|L]) :- member(X,L).', PrologGlobalInfo.ClauseAdded],
		['unique_list([]).', PrologGlobalInfo.ClauseAdded],
		['unique_list([X|L]) :- \\+ member(X, L), unique_list(L).', PrologGlobalInfo.ClauseAdded],
		[
			'crossword(V1, V2, V3, H1, H2, H3) :- word(V1, _, V12, _, V14, _, V16, _), word(V2, _, V22, _, V24, _, V26, _), word(V3, _, V32, _, V34, _, V36, _), word(H1, _, V12, _, V22, _, V32, _), word(H2, _, V14, _, V24, _, V34, _), word(H3, _, V16, _, V26, _, V36, _), unique_list([V1, V2, V3, H1, H2, H3]).',
			PrologGlobalInfo.ClauseAdded
		],
		[
			'?- crossword(V1, V2, V3, H1, H2, H3).',
			[
				'Satisfying substitution is: [H1 -> astoria; H2 -> baratto; H3 -> statale; V1 -> astante; V2 -> cobalto; V3 -> pistola]',
				'Satisfied'
			]
		]
	]);
});

test('LL(1) Prolog permutation test 1', () => {
	prologTest(
		[
			['append([], L, L).', PrologGlobalInfo.ClauseAdded],
			['append([X | Y], L, [X | Z]) :- append(Y, L, Z).', PrologGlobalInfo.ClauseAdded],
			['permutation([], []).', PrologGlobalInfo.ClauseAdded],
			[
				'permutation(L, [H|T]) :- append(V, [H|U], L), append(V, U, W), permutation(W, T).',
				PrologGlobalInfo.ClauseAdded
			],
			[
				'?- permutation([red, green, blue], C).',
				'Satisfying substitution is: [C -> [red, green, blue]]\n' +
					'Satisfying substitution is: [C -> [red, blue, green]]\n' +
					'Satisfying substitution is: [C -> [green, red, blue]]\n' +
					'Satisfying substitution is: [C -> [green, blue, red]]\n' +
					'Satisfying substitution is: [C -> [blue, red, green]]\n' +
					'Satisfying substitution is: [C -> [blue, green, red]]\n' +
					'Number of solutions found: 6\n' +
					PrologGlobalInfo.Satisfied +
					'\n'
			]
		],
		{ findAllSolutions: true }
	);
});

test('LL(1) Prolog factorial test 1', () => {
	// sub(N, 1, N1) means: N1 = N - 1
	prologTest([
		['factorial(0, 1).', PrologGlobalInfo.ClauseAdded],
		[
			// Correct (infix) operator syntax: N - 1
			'factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.',

			PrologGlobalInfo.ClauseAdded
		],
		['?- gt(1, 0).', [PrologGlobalInfo.Satisfied]],
		['?- 1 > 0.', [PrologGlobalInfo.Satisfied]],
		['?- sub(1, 1, 0).', [PrologGlobalInfo.Satisfied]],
		['?- sub(8, 5, 3).', [PrologGlobalInfo.Satisfied]],
		['?- factorial(1, 1).', [PrologGlobalInfo.Satisfied]],
		['?- factorial(2, 2).', [PrologGlobalInfo.Satisfied]],
		['?- factorial(3, 6).', [PrologGlobalInfo.Satisfied]],
		['?- factorial(4, 24).', [PrologGlobalInfo.Satisfied]],
		['?- factorial(5, 120).', [PrologGlobalInfo.Satisfied]]
	]);
});

test('LL(1) Prolog list length test', () => {
	// sub(N, 1, N1) means: N1 = N - 1
	prologTest([
		['length([], 0).', PrologGlobalInfo.ClauseAdded],
		['length([_ | T], L) :- length(T, K), add(K, 1, L).', PrologGlobalInfo.ClauseAdded],
		['?- length([], 0).', [PrologGlobalInfo.Satisfied]],
		['?- length([2], 1).', [PrologGlobalInfo.Satisfied]],
		['?- length([2, 3, 5, 7], 4).', [PrologGlobalInfo.Satisfied]],
		['?- length([2, 3, 5, 7], N).', success('N -> 4')]
	]);
});

test('LL(1) Prolog not test', () => {
	prologTest([
		['foo(X) :- \\+ bar(X).', PrologGlobalInfo.ClauseAdded],
		['bar(7).', PrologGlobalInfo.ClauseAdded],
		['?- foo(7).', [PrologGlobalInfo.NotSatisfied]],
		['?- foo(13).', [PrologGlobalInfo.Satisfied]]
	]);
});

test('LL(1) Prolog basic cut test', () => {
	// C# version of this test: 2014/03/08

	// Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- G(_).")); // Before 2014/03/13 : This assert fails; the query is satisfied.

	// 2014/03/13 : Fixed: See PrologVariable.Unify(); we no longer create bindings such as { X = _ }

	prologTest([
		['g(X) :- h(X), !, i(X).', PrologGlobalInfo.ClauseAdded],
		['g(20).', PrologGlobalInfo.ClauseAdded],
		['h(7).', PrologGlobalInfo.ClauseAdded],
		['h(13).', PrologGlobalInfo.ClauseAdded],
		['i(13).', PrologGlobalInfo.ClauseAdded],
		['?- g(X).', [PrologGlobalInfo.NotSatisfied]],
		['?- g(_).', [PrologGlobalInfo.NotSatisfied]]
	]);
});

test('LL(1) Prolog cut in query test', () => {
	// 2014/04/10.  See the last part of exercise 1 on http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse46
	prologTest(
		[
			['p(1).', PrologGlobalInfo.ClauseAdded],
			['p(2) :- !.', PrologGlobalInfo.ClauseAdded],
			['p(3).', PrologGlobalInfo.ClauseAdded],
			[
				'?- p(X), !, p(Y).',
				[
					'Satisfying substitution is: [X -> 1; Y -> 1]',
					'Satisfying substitution is: [X -> 1; Y -> 2]',
					'Number of solutions found: 2',
					PrologGlobalInfo.Satisfied
				]
			]
		],
		{ findAllSolutions: true }
	);
});

test('LL(1) Prolog notEqual via cut test', () => {
	// From Kamin section 8.5.2, page 387.
	prologTest([
		['equal(X, X).', PrologGlobalInfo.ClauseAdded],
		['notEqual(X, Y) :- equal(X, Y), !, fail.', PrologGlobalInfo.ClauseAdded],
		['notEqual(X, Y).', PrologGlobalInfo.ClauseAdded],
		['?- notEqual(7, 7).', [PrologGlobalInfo.NotSatisfied]],
		['?- notEqual(7, 13).', [PrologGlobalInfo.Satisfied]]
	]);
});

// **** Tests from the C# version ****

// [Test]
// public void NoArgumentsTest()
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("pred1."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("pred3(cons)."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- pred1."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- predUndefined."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- pred1(nil)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- pred3(cons)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- pred3(cons2)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- pred3(cons, nil)."));
// }
//
// [Test]
// public void ListMemberTest()    // Adapted from Kamin, page 354
// {
// 	// Do not use the member2 preset because this test uses non-standard list notation.
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("member(X, cons(X, L))."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("member(X, cons(Y, M)) :- member(X, M)."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- member(3, cons(2, cons(3, nil)))."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- member(3, cons(2, cons(4, nil)))."));
// }
//
// [Test]
// public void IncDecTest()
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("increment(X, Y) :- Y is X + 1."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("decrement(X, Y) :- Y is X - 1."));
//
// 	Assert.AreEqual("Y = 14\r\n" + satisfied, globalInfo.ProcessInputString("?- increment(13, Y)."));
// 	Assert.AreEqual("Y = 12\r\n" + satisfied, globalInfo.ProcessInputString("?- decrement(13, Y)."));
// }
//
// [Test]
// public void ComparisonTest()
// {
// 	// <
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 0 < 1."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 0 < 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 1 < 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- X < 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 0 < X."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- (((1 + 1) + 1) + 1) + 1 < 7 - 1."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- mod(7, 3) + 1 < mod(8, 3) + 1."));
//
// 	// >
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 0 > 1."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 0 > 0."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 1 > 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- X > 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 0 > X."));
//
// #if DEAD_CODE
// 	// <=
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 0 <= 1."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 0 <= 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 1 <= 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- X <= 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 0 <= X."));
// #else
// 	// =<
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 0 =< 1."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 0 =< 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 1 =< 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- X =< 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 0 =< X."));
// #endif
//
// 	// >=
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 0 >= 1."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 0 >= 0."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 1 >= 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- X >= 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 0 >= X."));
//
// 	// =:= (Arithmetic equals)
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 0 =:= 1."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 0 =:= 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 1 =:= 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- X =:= 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 0 =:= X."));
//
// 	// =\= (Arithmetic not equals)
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- 0 =\= 1."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString(@"?- 0 =\= 0."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- 1 =\= 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString(@"?- X =\= 0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString(@"?- 0 =\= X."));
// }
//
// [Test]
// public void FactorialTest1()
// {
// 	// From Fisher, section 2.2; see http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_2.html
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("factorial(0,1)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("factorial(N,F) :- N > 0, N1 is N - 1, factorial(N1,F1), F is N * F1."));
//
// 	Assert.AreEqual("W = 6\r\n" + satisfied, globalInfo.ProcessInputString("?- factorial(3,W)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- factorial(3,6)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- factorial(5,2)."));
// }
//
// [Test]
// public void FactorialTest2()
// {
// 	// From Fisher, section 2.2
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("factorial(0,F,F)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("factorial(N,A,F) :- N > 0, A1 is N * A, N1 is N - 1, factorial(N1,A1,F)."));
//
// 	Assert.AreEqual("F = 120\r\n" + satisfied, globalInfo.ProcessInputString("?- factorial(5,1,F)."));
// }
//
// [Test]
// public void NonBindingVarTest()
// {
// 	// See Fisher, section 2.3
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("foo(7, 13)."));
//
// 	//Assert.AreEqual("X = 7, _ = _\r\n" + satisfied, globalInfo.ProcessInputString("?- foo(X, _), print(X, _)."));
// 	Assert.AreEqual("X = 7\r\n" + satisfied, globalInfo.ProcessInputString("?- foo(X, _)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- foo(X, _), write(_).")); // Fails because _ is not bound.
// }
//
// [Test]
// public void WriteTest()
// {
// 	Assert.AreEqual("This is\r\nsome text.\r\n" + satisfied, globalInfo.ProcessInputString("?- write('This is'), nl, write('some text.')."));
// }
//
// #if DEAD_CODE
// [Test]
// public void GoalIsomorphismTest()
// {
// 	var clause1 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo(X, Y)."));
// 	var clause2 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo(A, B)."));
// 	var clause3 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo(Z, Z)."));
//
// 	Assert.IsTrue(clause1.Lhs.IsIsomorphicTo(clause2.Lhs));
// 	Assert.IsFalse(clause1.Lhs.IsIsomorphicTo(clause3.Lhs));
// }
// #endif
//
// [Test]
// public void ClauseIsomorphismTest()
// {
// 	var clause1 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo(X, Y)."));
// 	var clause2 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo(A, B)."));
// 	var clause3 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo(Z, Z)."));
// 	var clause4 = (PrologClause)parser.Parse(tokenizer.Tokenize("connected(X, Y) :- edge(X, Y)."));
// 	var clause5 = (PrologClause)parser.Parse(tokenizer.Tokenize("connected(X, Y) :- edge(Y, X)."));
//
// 	Assert.IsTrue(clause1.IsIsomorphicTo(clause2, null, globalInfo));
// 	Assert.IsFalse(clause1.IsIsomorphicTo(clause3, null, globalInfo));
// 	Assert.IsFalse(clause4.IsIsomorphicTo(clause5, null, globalInfo));
// }
//
// [Test]
// public void AssertTest()
// {
// 	// 1
// 	Assert.AreEqual("X = 7\r\n" + satisfied, globalInfo.ProcessInputString("?- assert(foo(7)), foo(X)."));
//
// 	// 2
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("bar(13)."));
// 	Assert.AreEqual("X = 13\r\n" + satisfied, globalInfo.ProcessInputString("?- assert((bat(Y) :- bar(Y))), bat(X)."));
//
// 	// 3
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- assert((X :- Y, Z(W)))."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- assert((X(W) :- Y, Z(W)))."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("Y."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("Z(7)."));
// 	Assert.AreEqual("W = 7\r\n" + satisfied, globalInfo.ProcessInputString("?- X(W)."));
// }
//
// [Test]
// public void RetractTest()
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("foo(7)."));
// 	Assert.AreEqual("X = 7\r\n" + satisfied, globalInfo.ProcessInputString("?- foo(X)."));
// 	Assert.AreEqual("X = 7\r\n" + satisfied, globalInfo.ProcessInputString("?- retract(foo(X))."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- foo(X)."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("bar(13)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("bat(Z) :- bar(Z)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- retract((bat(Y) :- bar(Y))), bat(X)."));
// }
//
// [Test]
// public void NotTest1()
// {
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- not(foo(X))."));
// }
//
// [Test]
// public void NotTest2()
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("foo(7)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- not(foo(X))."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- not(foo(7))."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- not(foo(13))."));
// }
//
// [Test]
// public void NotTest3() // Test "not" on goals that look like variables.
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("X."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- not(X)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- not(Y)."));
// }
//
// [Test]
// public void NotSymbolTest()
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("bar(7)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"foo(X) :- \+ bar(X)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- foo(7)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- foo(13)."));
// }
//
// [Test]
// public void UnificationTest()
// {
// 	var x = new PrologVariable("X");
// 	var clause1 = (PrologClause)parser.Parse(tokenizer.Tokenize("baz(Z) :- assert((foo(Z) :- bat(Z)))."));
// 	var clause2 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo(Y) :- not(bar(Y))."));
// 	var clause3 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo([1,2,3])."));
// 	var clause4 = (PrologClause)parser.Parse(tokenizer.Tokenize("baz(7) :- assert((foo(7) :- bat(7)))."));
// 	var clause5 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo(13) :- not(bar(13))."));
// 	var goal = clause2.Lhs;
// 	var goalWithInnerGoal = clause2.Rhs[0];
// 	var goalWithInnerClause = clause1.Rhs[0];
// 	var functorExpression = clause3.Lhs.ExpressionList[0];
//
// 	Assert.IsNotNull(x.Unify(functorExpression));
// 	Assert.IsNotNull(functorExpression.Unify(x));
// 	Assert.IsNull(x.Unify(goal));
// 	Assert.IsNull(goal.Unify(x));
// 	Assert.IsNull(x.Unify(goalWithInnerGoal));
// 	Assert.IsNull(goalWithInnerGoal.Unify(x));
// 	Assert.IsNull(x.Unify(goalWithInnerClause));
// 	Assert.IsNull(goalWithInnerClause.Unify(x));
// 	Assert.IsNull(x.Unify(clause3));
// 	Assert.IsNull(clause3.Unify(x));
// 	Assert.IsNotNull(clause2.Lhs.Unify(clause3.Lhs));
// 	Assert.IsNotNull(clause3.Lhs.Unify(clause2.Lhs));
// 	Assert.IsNotNull(clause1.Unify(clause4));
// 	Assert.IsNotNull(clause4.Unify(clause1));
// 	Assert.IsNotNull(clause2.Unify(clause5));
// 	Assert.IsNotNull(clause5.Unify(clause2));
// }
//
// [Test]
// public void IsomorphicOrMoreGeneralClauseAddedTest()
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("foo(X)."));
// 	Assert.AreEqual(PrologGlobalInfo.IsomorphicClauseAlreadyExists, globalInfo.ProcessInputString("foo(Y)."));
// 	Assert.AreEqual(PrologGlobalInfo.IsomorphicOrMoreGeneralClauseAlreadyExists, globalInfo.ProcessInputString("foo(7)."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("bar(7)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("bar(13)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("bar(X)."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("baz(X, Y)."));
// 	Assert.AreEqual(PrologGlobalInfo.IsomorphicOrMoreGeneralClauseAlreadyExists, globalInfo.ProcessInputString("baz(Z, Z)."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("bat(Z, Z)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("bat(X, Y)."));
// }
//
// [Test]
// public void ClauseUnificationTest()
// {
// 	var clause1 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo(X, Y)."));
// 	var clause2 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo(Z, Z)."));
// 	var clause3 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo(X, Y) :- bar(X, Z), bat(Z, Y)."));
// 	var clause4 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo(A, b) :- bar(A, c), bat(c, b)."));
// 	var clause5 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo(A, b) :- bar(A, c), bat(c, b), baz(D)."));
// 	var clause6 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo(A, b) :- bar2(A, c), bat(c, b)."));
// 	var clause7 = (PrologClause)parser.Parse(tokenizer.Tokenize("foo(A, b) :- bar(A, c), bat(c, d)."));
//
// 	var unifier1_2 = clause1.Unify(clause2);
// 	var unifier3_4 = clause3.Unify(clause4);
//
// 	Assert.IsNotNull(unifier1_2);
// 	Assert.IsTrue(clause1.ApplySubstitution(unifier1_2).Equals(clause2.ApplySubstitution(unifier1_2)));
//
// 	Assert.IsNotNull(unifier3_4);
// 	Assert.IsTrue(clause3.ApplySubstitution(unifier3_4).Equals(clause4.ApplySubstitution(unifier3_4)));
//
// 	Assert.IsNull(clause3.Unify(clause5));
// 	Assert.IsNull(clause3.Unify(clause6));
// 	Assert.IsNull(clause3.Unify(clause7));
// }
//
// [Test]
// public void ListNotationTest()
// {
// 	globalInfo.LoadPreset("member");
//
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- member(2, [])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- member(2, [1, 2, 3])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- member(4, [1, 2, 3])."));
//
// 	globalInfo.FindAllSolutions();
//
// 	Assert.AreEqual(@"X = 2;
// X = 3;
// X = 5;
// X = 7;
// " + notSatisfied, globalInfo.ProcessInputString("?- member(X, [2, 3, 5, 7])."));
// }
//
// [Test]
// public void SequenceNotationTest()  // See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_7.html
// {
// 	// ThAW 2014/03/28 : I added brackets around sequences because without them, ?- X = [(1, 2), (3, 4)], print(X). yielded [1, 2, 3, 4],
// 	// which was misleading.
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("foo((1, 2, 3, 4))."));
// 	Assert.AreEqual("H = 1, T = (2, 3, 4)\r\n" + satisfied, globalInfo.ProcessInputString("?- foo((H, T))."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("equal(X, X)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- equal((a), a)."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("bar((a))."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- bar((H, T))."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("baz((1, 2, 3, 4, 5))."));
// 	Assert.AreEqual("A = 1, B = 2, C = (3, 4, 5)\r\n" + satisfied, globalInfo.ProcessInputString("?- baz((A, B, C))."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("sequence_append((X,R),S,(X,T)) :- !, sequence_append(R,S,T)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("sequence_append((X),S,(X,S))."));
// 	Assert.AreEqual("S = (1, 2, 3, a, b, c, d)\r\n" + satisfied, globalInfo.ProcessInputString("?- sequence_append((1,2,3),(a,b,c,d),S)."));
// }
//
// [Test]
// public void ArithmeticExpressionTest()
// {
// 	Assert.AreEqual("X = 16\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 5 * 3 + 4 / 2 - 1."));
// 	Assert.AreEqual("X = 7\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 10 - 2 - 1."));
// 	Assert.AreEqual("X = 25\r\n" + satisfied, globalInfo.ProcessInputString("?- X is (2 + 3) * 5."));
// 	Assert.AreEqual("X = 26\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 5 * (2 + 3) + 1."));
// 	Assert.AreEqual("X = 3\r\n" + satisfied, globalInfo.ProcessInputString("?- X is mod(29, 7) + mod(11, 3)."));
// 	Assert.AreEqual("X = 5\r\n" + satisfied, globalInfo.ProcessInputString("?- X is (((1 + 1) + 1) + 1) + 1."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 5 is (((1 + 1) + 1) + 1) + 1."));
// }
//
// [Test]
// public void GoalDisjunctionTest()
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("bar(7, 13)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("foo(X, Y) :- bar(X, Y) ; bar(Y, X)."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- foo(7, 13)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- foo(13, 7)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- foo(0, 1)."));
// }
//
// [Test]
// public void GoalIfThenElseTest()    // Adapted from http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_9.html
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("member(X,[X|_])."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("member(X,[_|L]) :- member(X,L)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("find_regions([],R,R). "));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
// find_regions([[X,Y]|S], R,A) :-
// (member(X,R) ->
// (member(Y,R) -> find_regions(S,R,A) : find_regions(S,[Y|R],A)) :
//    (member(Y,R) -> find_regions(S,[X|R],A) : find_regions(S,[X,Y|R],A) ) )."));
//
// 	Assert.AreEqual("R = [5, 4, 3, 1, 2]\r\n" + satisfied,
// 		globalInfo.ProcessInputString("?- find_regions([[1,2],[1,3],[1,4],[1,5],[2,3],[2,4],[3,4],[4,5]],[],R)."));
// }
//
// [Test]
// public void UnificationOperatorTest() // Test the = (unification) infix operator.  The LHS may be a functor only if the LL(1) grammar is used.
// {
// 	Assert.AreEqual("X = [1, 2, 3]\r\n" + satisfied, globalInfo.ProcessInputString("?- X = [1, 2, 3]."));
// 	Assert.AreEqual("X = t\r\n" + satisfied, globalInfo.ProcessInputString("?- mia(X) = mia(t)."));
// }
//
// [Test]
// public void EqualityTest()
// {
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 7 == 7."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 7 == 13."));
// 	Assert.AreEqual("X = [1, 2, 3]\r\n" + satisfied, globalInfo.ProcessInputString("?- X = [1, 2, 3], X == [1, 2, 3]."));
// #if DEAD_CODE
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- X == X.")); // X is unbound.
// #else
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse37
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- X == X."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- X == Y."));
// 	Assert.AreEqual("X = Y\r\n" + satisfied, globalInfo.ProcessInputString("?- X = Y, X == Y."));
// #endif
// }
//
// [Test]
// public void NotEqualTest()
// {
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString(@"?- X \== X."));
// #if DEAD_CODE
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString(@"?- X \== Y."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString(@"?- X \== 1."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString(@"?- 1 \== X."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString(@"?- X \== red."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString(@"?- red \== X."));
// #else
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse37
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- X \== Y."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- X \== 1."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- 1 \== X."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- X \== red."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- red \== X."));
// #endif
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString(@"?- 1 \== 1."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString(@"?- red \== red."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- 0 \== 1."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- red \== green."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- 1 \== red."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- red \== 1."));
// }
//
// [Test]
// public void ConnectedTest() // See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_15.html
// {
// #if !DEAD_CODE
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("edge(1, 2)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("connected(X, Y) :- edge(X, Y)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("connected(X, Y) :- edge(Y, X)."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- connected(2, 1)."));
// #else
// 	var clause1 = (PrologClause)parser.Parse(tokenizer.Tokenize("connected(X, Y) :- edge(X, Y)."));
// 	var clause2 = (PrologClause)parser.Parse(tokenizer.Tokenize("connected(X, Y) :- edge(Y, X)."));
// 	// Before unifying, we must ensure that the two clauses do not contain any variables in common.
// 	var unifier = clause2.Unify(clause1);
//
// 	Assert.IsNotNull(unifier);
// 	Assert.IsTrue(unifier.IsOneToOne);
//
// 	throw new Exception(unifier.ToString());
// #endif
// }
//
// [Test]
// public void RetractAllTest()
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("foo(1)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("foo(2)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("foo(3)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- foo(_)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- retractall(foo(_))."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString(@"?- foo(_)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString(@"?- retractall(foo(_))."));    // There is now nothing to retract.
// }
//
// [Test]
// public void MapColouringCutTest() // 2014/03/07
// {
// 	// Map colouring redux - Exercise 2.9 - Optimization - March 3, 2014
// 	// See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_9.html
// #if DEAD_CODE
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("member(X,[X|_])."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("member(X,[_|L]) :- member(X,L)."));
// #else
// 	globalInfo.LoadPreset("member");
// #endif
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("adjacent(X,Y,Map) :- member([X,Y],Map) ; member([Y,X],Map)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("find_regions([],R,R)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
// find_regions([[X,Y]|S], R,A) :-
// (member(X,R) ->
// (member(Y,R) -> find_regions(S,R,A) : find_regions(S,[Y|R],A)) :
//    (member(Y,R) -> find_regions(S,[X|R],A) : find_regions(S,[X,Y|R],A) ) )."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
// conflict(Map,[[R1,C]|ColoringSoFar]) :-
// member([R2,C],ColoringSoFar),
// adjacent(R1,R2,Map)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("color_all([],_,[],_,_)."));
// #if DEAD_CODE
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
// color_all([R|Rs],Colors,[[R,C]|A],Map,ColoringSoFar) :-
// member(C,Colors),
// \+ conflict(Map,[[R,C]|ColoringSoFar]),
// color_all(Rs,Colors,A,Map,[[R,C]|ColoringSoFar])."));
// #else
// 	// Use a cut, rather than \+, to implement negation.  This is a good test of our interpreter's cut implementation.
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
// color_all([R|Rs],Colors,[[R,C]|A],Map,ColoringSoFar) :-
// member(C,Colors),
// not_conflict(Map,[[R,C]|ColoringSoFar]),
// color_all(Rs,Colors,A,Map,[[R,C]|ColoringSoFar])."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("not_conflict(Map, Coloring) :- conflict(Map, Coloring), !, fail."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("not_conflict(_, _)."));
// #endif
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
// color(Map,Colors,Coloring) :-
// find_regions(Map,[],Regions),
// color_all(Regions,Colors,Coloring,Map,[])."));
//
// 	Assert.AreEqual("Coloring = [[5, red], [4, green], [3, red], [1, blue], [2, yellow]]\r\n" + satisfied,
// 		globalInfo.ProcessInputString(@"?- color([[1, 2], [1, 3], [1, 4], [1, 5], [2, 3], [2, 4], [3, 4], [4, 5]],
// 			[red, green, blue, yellow], Coloring)."));
// }
//
// [Test]
// public void BasicCutTest() // 2014/03/08
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("G(X) :- H(X), !, I(X)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("G(20)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("H(7)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("H(13)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("I(13)."));
//
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- G(X)."));
//
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- G(_).")); // Before 2014/03/13 : This assert fails; the query is satisfied.
// 	// My guess is that G(_) unifies with G(X) via the substitution X = _
// 	// Then it satisfies G(_) via the rule by satisfying H(_) and I(_) via H(7) and I(13).
// 	// This illustrates a potential hazard relating to the use of non-binding variables.
// 	// Does real Prolog suffer from this same hazard?
// 	// 2014/03/13 : Fixed: See PrologVariable.Unify(); we no longer create bindings such as { X = _ }
// }
//
// [Test]
// public void OccursCheckTest() // 2014/03/12
// {
// 	// For a discussion of the "occurs" check in Prolog unification,
// 	// see http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse5
// 	// According to that discussion, we have implemented standard unification (with the "occurs" check), not true Prolog unification.
// 	// Our unification algorithm is safer, but slower (because of the "occurs" check), than true Prolog unification.
// 	// Our predicate "unifiable/2" is equivalent to true Prolog's predicate "unify_with_occurs_check/2".
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- unifiable(X, father(X))."));
// }
//
// [Test]
// public void AppendTest() // 2014/03/13
// {
// 	globalInfo.LoadPreset("append");
// 	globalInfo.FindAllSolutions();
//
// 	Assert.AreEqual(@"X = [], Y = [2, 3, 5, 7];
// X = [2], Y = [3, 5, 7];
// X = [2, 3], Y = [5, 7];
// X = [2, 3, 5], Y = [7];
// X = [2, 3, 5, 7], Y = [];
// " + notSatisfied, globalInfo.ProcessInputString("?- append(X, Y, [2, 3, 5, 7])."));
// }
//
// [Test]
// public void ArithExprAsGeneralExprTest() // 2014/03/24; test the usage of an arithmetic expression as a general expression.
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("foo(1 + 1).")); // Essentially foo(+(1, 1)).
// 	Assert.AreEqual("X = +(1, 1), Y = 2\r\n" + satisfied, globalInfo.ProcessInputString("?- foo(X), Y is X."));
// }
//
// [Test]
// public void DoNotRenameNonBindingVariablesTest() // 2014/03/28
// {
// 	// Learn Prolog Now! Exercise 6.6 - A logic puzzle - March 28, 2014
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse26
//
// 	// The street is a list of three houses.
// 	// A house is a list: [colour, nationality, pet]
// 	globalInfo.LoadPreset("member");
// 	globalInfo.LoadPreset("append");
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("prefix(P,L) :- append(P,_,L)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("suffix(S,L) :- append(_,S,L)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("sublist(SubL,L) :- suffix(S,L), prefix(SubL,S)."));
// #if DEAD_CODE
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("Permutation([], [])."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("Permutation(L, [H|T]) :- append(V, [H|U], L), append(V, U, W), Permutation(W, T)."));
// #else
// 	globalInfo.LoadPreset("permutation");
// #endif
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
// Foo1(C,N,P) :-
// permutation([red, green, blue], C),
// permutation([englishman, spaniard, japanese], N),
// permutation([jaguar, snail, zebra], P)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("Transform([],[],[],[])."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("Transform([CH|CT],[NH|NT],[PH|PT],[[CH,NH,PH]|ST]) :- Transform(CT,NT,PT,ST)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("Constraint1(L) :- member([red,englishman,_],L)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("Constraint2(L) :- member([_,spaniard,jaguar],L)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("Constraints3And4(L) :- sublist([[_,_,snail],[blue,japanese,_]],L)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString(@"
// zebra(N) :-
// Foo1(CL,NL,PL),
// Transform(CL,NL,PL,Street),
// Constraint1(Street),
// Constraint2(Street),
// Constraints3And4(Street),
// member([_, N, zebra], Street). % This used to fail because _ was being renamed to a bound variable, for which an incorrect value was then substituted."));
//
// 	Assert.AreEqual("N = japanese\r\n" + satisfied, globalInfo.ProcessInputString("?- zebra(N)."));
// }
//
// [Test]
// public void DoubledListTest() // 2014/03/31
// {
// 	// Learn Prolog Now! Exercise 6.1
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse26
//
// 	globalInfo.LoadPreset("append");
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("doubled(List) :- append(X,X,List)."));
//
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- doubled([a, b, c, a, b])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- doubled([a, b, c, a, b, c])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- doubled([a, b, c, a, b, c, a])."));
// }
//
// [Test]
// public void PalindromeTest() // 2014/03/31
// {
// 	// Learn Prolog Now! Exercise 6.2
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse26
//
// #if DEAD_CODE
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("accRev([H|T],A,R):-  accRev(T,[H|A],R)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("accRev([],A,A)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("rev(L,R) :- accRev(L,[],R)."));
// #else
// 	globalInfo.LoadPreset("rev");
// #endif
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("palindrome(List) :- rev(List,List)."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- palindrome([r, o, t, a, t, o, r])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- palindrome([n, u, r, s, e, s, r, u, n])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- palindrome([n, o, t, h, i, s])."));
// }
//
// [Test]
// public void TopTailTest() // 2014/03/31
// {
// 	// Learn Prolog Now! Exercise 6.3
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse26
//
// 	globalInfo.LoadPreset("append");
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("toptail(InList,OutList) :- append([_],OutList,X), append(X,[_],InList)."));
//
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- toptail([a], T)."));
// 	Assert.AreEqual("T = []\r\n" + satisfied, globalInfo.ProcessInputString("?- toptail([a, b], T)."));
// 	Assert.AreEqual("T = [b]\r\n" + satisfied, globalInfo.ProcessInputString("?- toptail([a, b, c], T)."));
// }
//
// [Test]
// public void Last1Test() // 2014/03/31
// {
// 	// Learn Prolog Now! Exercise 6.4 part 1
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse26
//
// 	globalInfo.LoadPreset("rev");
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("last(List, X) :- rev(List, [X | _])."));
//
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- last([], _)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- last([a], b)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- last([a], a)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- last([a, b, c], a)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- last([a, b, c], b)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- last([a, b, c], c)."));
// }
//
// [Test]
// public void Last2Test() // 2014/03/31
// {
// 	// Learn Prolog Now! Exercise 6.4 part 2
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse26
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("last([X], X)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("last([_ | L], X) :- last(L, X)."));
//
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- last([], _)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- last([a], b)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- last([a], a)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- last([a, b, c], a)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- last([a, b, c], b)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- last([a, b, c], c)."));
// }
//
// [Test]
// public void SwapFL1Test() // 2014/03/31
// {
// 	// Learn Prolog Now! Exercise 6.5 part 1
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse26
//
// 	globalInfo.LoadPreset("append");
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("swapfl([H1 | T1], [H2 | T2]) :- append(L, [H2], T1), append(L, [H1], T2)."));
//
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- swapfl([a, b], [b, c])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- swapfl([a, b], [b, a])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- swapfl([a, b, c, d, e], [a, b, c, d, e])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- swapfl([a, b, c, d, e], [e, d, c, b, a])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- swapfl([a, b, c, d, e], [e, b, c, d, a])."));
// }
//
// [Test]
// public void SwapFL2Test() // 2014/03/31
// {
// 	// Learn Prolog Now! Exercise 6.5 part 2
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse26
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("swapfl([H1 | T1], [H2 | T2]) :- swapflHelper(H1, H2, T1, T2)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("swapflHelper(H1, H2, [H2], [H1])."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("swapflHelper(H1, H2, [X | T1], [X | T2]) :- swapflHelper(H1, H2, T1, T2)."));
//
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- swapfl([a, b], [b, c])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- swapfl([a, b], [b, a])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- swapfl([a, b, c, d, e], [a, b, c, d, e])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- swapfl([a, b, c, d, e], [e, d, c, b, a])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- swapfl([a, b, c, d, e], [e, b, c, d, a])."));
// }
//
// [Test]
// public void CFGDiffListsTest() // 2014/03/31
// {
// 	// Context-free grammar recognition using difference lists
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse28
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("s(X,Z) :- np(X,Y), vp(Y,Z)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("np(X,Z) :- det(X,Y), n(Y,Z)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("vp(X,Z) :- v(X,Y), np(Y,Z)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("vp(X,Z) :- v(X,Z)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("det([the|W],W)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("det([a|W],W)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("n([woman|W],W)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("n([man|W],W)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("v([loves|W],W)."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- s([a, woman, loves, a, man], [])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- s([the, woman, loves, a, man], []).")); // Irene Adler.
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- s([woman, a, woman, man, a, loves], [])."));
//
// 	globalInfo.FindAllSolutions();
// 	Assert.AreEqual(@"X = [the, woman, loves, the, woman];
// X = [the, woman, loves, the, man];
// X = [the, woman, loves, a, woman];
// X = [the, woman, loves, a, man];
// X = [the, woman, loves];
// X = [the, man, loves, the, woman];
// X = [the, man, loves, the, man];
// X = [the, man, loves, a, woman];
// X = [the, man, loves, a, man];
// X = [the, man, loves];
// X = [a, woman, loves, the, woman];
// X = [a, woman, loves, the, man];
// X = [a, woman, loves, a, woman];
// X = [a, woman, loves, a, man];
// X = [a, woman, loves];
// X = [a, man, loves, the, woman];
// X = [a, man, loves, the, man];
// X = [a, man, loves, a, woman];
// X = [a, man, loves, a, man];
// X = [a, man, loves];
// " + notSatisfied, globalInfo.ProcessInputString("?- s(X, []).")); // Generate all 20 sentences in the language.
// }
//
// [Test]
// public void DCGTest1() // 2014/04/01
// {
// 	// Test of the interpreter's Definite Clause Grammar support.
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse29
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("s --> np, vp."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("np --> det, n."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("vp --> v, np."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("vp --> v."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("det --> [the]."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("det --> [a]."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("n --> [woman]."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("n --> [man]."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("v --> [loves]."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- s([a, woman, loves, a, man], [])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- s([the, woman, loves, a, man], []).")); // Irene Adler.
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- s([woman, a, woman, man, a, loves], [])."));
//
// 	globalInfo.FindAllSolutions();
// 	Assert.AreEqual(@"X = [the, woman, loves, the, woman];
// X = [the, woman, loves, the, man];
// X = [the, woman, loves, a, woman];
// X = [the, woman, loves, a, man];
// X = [the, woman, loves];
// X = [the, man, loves, the, woman];
// X = [the, man, loves, the, man];
// X = [the, man, loves, a, woman];
// X = [the, man, loves, a, man];
// X = [the, man, loves];
// X = [a, woman, loves, the, woman];
// X = [a, woman, loves, the, man];
// X = [a, woman, loves, a, woman];
// X = [a, woman, loves, a, man];
// X = [a, woman, loves];
// X = [a, man, loves, the, woman];
// X = [a, man, loves, the, man];
// X = [a, man, loves, a, woman];
// X = [a, man, loves, a, man];
// X = [a, man, loves];
// " + notSatisfied, globalInfo.ProcessInputString("?- s(X, []).")); // Generate all 20 sentences in the language.
// }
//
// [Test]
// public void DCGExtraArgsTest() // 2014/04/01
// {
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse32
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("s  -->  np(subject),vp."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("np(_)  -->  det,n."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("np(X)  -->  pro(X)."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("vp  -->  v,np(object)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("vp  -->  v."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("det  -->  [the]."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("det  -->  [a]."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("n  -->  [woman]."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("n  -->  [man]."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("pro(subject)  -->  [he]."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("pro(subject)  -->  [she]."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("pro(object)  -->  [him]."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("pro(object)  -->  [her]."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("v  -->  [loves]."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- s([a, woman, loves, a, man], [])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- s([a, woman, loves, him], [])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- s([she, loves, a, man], [])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- s([she, loves, him], [])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- s([a, woman, loves], [])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- s([she, loves], [])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- s([him, loves, she], [])."));
// }
//
// [Test]
// public void DCGParseTreeTest() // 2014/04/01
// {
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse32
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("s(s(NP,VP))  -->  np(NP),vp(VP)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("np(np(DET,N))  -->  det(DET),n(N)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("vp(vp(V,NP))  -->  v(V),np(NP)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("vp(vp(V))        -->  v(V)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("det(det(the))  -->  [the]."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("det(det(a))      -->  [a]."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("n(n(woman))  -->  [woman]."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("n(n(man))      -->  [man]."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("v(v(loves))  -->  [loves]."));
//
// 	Assert.AreEqual(@"T = s(np(det(a), n(woman)), vp(v(loves), np(det(a), n(man))))
// " + satisfied, globalInfo.ProcessInputString("?- s(T, [a, woman, loves, a, man], [])."));
// }
//
// [Test]
// public void DCGNonCFGTest() // 2014/04/01
// {
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse32
// 	// The following is not a context-free grammar:
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("s(Count)  -->  ablock(Count),bblock(Count),cblock(Count)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("ablock(0)  -->  []."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("ablock(succ(Count))  -->  [a],ablock(Count)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("bblock(0)  -->  []."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("bblock(succ(Count))  -->  [b],bblock(Count)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("cblock(0)  -->  []."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("cblock(succ(Count))  -->  [c],cblock(Count)."));
//
// 	Assert.AreEqual(@"L = []
// " + satisfied, globalInfo.ProcessInputString("?- s(0, L, [])."));
// 	Assert.AreEqual(@"L = [a, b, c]
// " + satisfied, globalInfo.ProcessInputString("?- s(succ(0), L, [])."));
// 	Assert.AreEqual(@"L = [a, a, b, b, c, c]
// " + satisfied, globalInfo.ProcessInputString("?- s(succ(succ(0)), L, [])."));
// 	Assert.AreEqual(@"L = [a, a, a, b, b, b, c, c, c]
// " + satisfied, globalInfo.ProcessInputString("?- s(succ(succ(succ(0))), L, [])."));
// }
//
// [Test]
// public void DCGLexiconTest() // 2014/04/01
// {
// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse33
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("lex(the,det)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("lex(a,det)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("lex(woman,n)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("lex(man,n)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("lex(loves,v)."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("s --> np, vp."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("np  -->  det,n."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("vp  -->  v,np."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("vp  -->  v."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("det  -->  [Word],{lex(Word,det)}."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("n  -->  [Word],{lex(Word,n)}."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("v  -->  [Word],{lex(Word,v)}."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- s([a, woman, loves, a, man], [])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- s([the, woman, loves, a, man], []).")); // Irene Adler.
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- s([woman, a, woman, man, a, loves], [])."));
//
// 	globalInfo.FindAllSolutions();
// 	Assert.AreEqual(@"X = [the, woman, loves, the, woman];
// X = [the, woman, loves, the, man];
// X = [the, woman, loves, a, woman];
// X = [the, woman, loves, a, man];
// X = [the, woman, loves];
// X = [the, man, loves, the, woman];
// X = [the, man, loves, the, man];
// X = [the, man, loves, a, woman];
// X = [the, man, loves, a, man];
// X = [the, man, loves];
// X = [a, woman, loves, the, woman];
// X = [a, woman, loves, the, man];
// X = [a, woman, loves, a, woman];
// X = [a, woman, loves, a, man];
// X = [a, woman, loves];
// X = [a, man, loves, the, woman];
// X = [a, man, loves, the, man];
// X = [a, man, loves, a, woman];
// X = [a, man, loves, a, man];
// X = [a, man, loves];
// " + notSatisfied, globalInfo.ProcessInputString("?- s(X, []).")); // Generate all 20 sentences in the language.
// }
//
// [Test]
// public void UnaryMinusTest() // 2014/04/02
// {
// 	Assert.AreEqual("X = 3\r\n" + satisfied, globalInfo.ProcessInputString("?- X is - -3."));
// 	Assert.AreEqual("X = 6\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 2 * - -3."));
// 	Assert.AreEqual("X = 5\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 2 + - -3."));
// 	Assert.AreEqual("X = 4\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 5 + - mod(1, 2)."));
// 	Assert.AreEqual("X = -1\r\n" + satisfied, globalInfo.ProcessInputString("?- X is - mod(1, 2)."));
// 	Assert.AreEqual("X = -5\r\n" + satisfied, globalInfo.ProcessInputString("?- X is - (2 + 3)."));
// 	Assert.AreEqual("X = 4\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 9 + - (2 + 3)."));
// 	Assert.AreEqual("X = 12\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 13 + - -(3, 2)."));
// 	Assert.AreEqual("X = 8\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 13 + - +(3, 2)."));
// 	Assert.AreEqual("X = 12\r\n" + satisfied, globalInfo.ProcessInputString("?- X is - -(3, 2) + 13."));
// 	Assert.AreEqual("X = 8\r\n" + satisfied, globalInfo.ProcessInputString("?- X is - +(3, 2) + 13."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("calculate(X, Y) :- Y is X."));
// 	Assert.AreEqual("Y = 12\r\n" + satisfied, globalInfo.ProcessInputString("?- calculate(- -(3, 2) + 13, Y)."));
// 	Assert.AreEqual("Y = 8\r\n" + satisfied, globalInfo.ProcessInputString("?- calculate(- +(3, 2) + 13, Y)."));
// 	Assert.AreEqual("Y = 19\r\n" + satisfied, globalInfo.ProcessInputString("?- calculate(7 + - -(3, 2) + 13, Y)."));
// 	Assert.AreEqual("Y = 15\r\n" + satisfied, globalInfo.ProcessInputString("?- calculate(7 + - +(3, 2) + 13, Y)."));
// 	Assert.AreEqual("Z = 7, Y = 19\r\n" + satisfied, globalInfo.ProcessInputString("?- Z = 7, calculate(Z + - -(3, 2) + 13, Y)."));
// 	Assert.AreEqual("Z = 7, Y = 15\r\n" + satisfied, globalInfo.ProcessInputString("?- Z = 7, calculate(Z + - +(3, 2) + 13, Y)."));
// 	Assert.AreEqual("Y = -14\r\n" + satisfied, globalInfo.ProcessInputString("?- calculate(-(-(3, 2) + 13), Y)."));
// 	Assert.AreEqual("Y = -18\r\n" + satisfied, globalInfo.ProcessInputString("?- calculate(-(+(3, 2) + 13), Y)."));
// 	Assert.AreEqual("Y = 24\r\n" + satisfied, globalInfo.ProcessInputString("?- calculate((- -(3, 2) + 13) * 2, Y)."));
// 	Assert.AreEqual("Y = 16\r\n" + satisfied, globalInfo.ProcessInputString("?- calculate((- +(3, 2) + 13) * 2, Y)."));
// 	Assert.AreEqual("Y = -24\r\n" + satisfied, globalInfo.ProcessInputString("?- calculate(-(- -(3, 2) + 13) * 2, Y)."));
// 	Assert.AreEqual("Y = -16\r\n" + satisfied, globalInfo.ProcessInputString("?- calculate(-(- +(3, 2) + 13) * 2, Y)."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- - 2 < -1."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- - 2 < -2."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- - 2 - 1 < -2."));
//
// 	Assert.AreEqual("X = 2\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 2, - X < -1."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- X is 2, - X < -2."));
// 	Assert.AreEqual("X = 2\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 2, - X - 1 < -2."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- - mod(2, 3) < -1."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- - mod(2, 3) < -2."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- - mod(2, 3) - 1 < -2."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- - (1 + 1) < -1."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- - (1 + 1) < -2."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- - (1 + 1) - 1 < -2."));
// }
//
// [Test]
// public void PrefixListNotationTest()
// {
// 	Assert.AreEqual("X = []\r\n" + satisfied, globalInfo.ProcessInputString("?- X = []."));
// 	Assert.AreEqual("X = [1]\r\n" + satisfied, globalInfo.ProcessInputString("?- X = .(1, [])."));
// 	Assert.AreEqual("X = [1, 2, 3]\r\n" + satisfied, globalInfo.ProcessInputString("?- X = .(1, [2, 3])."));
// 	Assert.AreEqual("X = [1, 2, 3]\r\n" + satisfied, globalInfo.ProcessInputString("?- .(1, [2, 3]) = X."));
// 	Assert.AreEqual("X = 1, Y = 2\r\n" + satisfied, globalInfo.ProcessInputString("?- .(1, [2, 3]) = [X, Y | _]."));
// }
//
// [Test]
// public void SingleQuotedIdentifierTest()
// {
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- a == 'a'."));
// 	Assert.AreEqual("X = []\r\n" + satisfied, globalInfo.ProcessInputString("?- X = '[]'."));
// 	Assert.AreEqual("X = []\r\n" + satisfied, globalInfo.ProcessInputString("?- '[]' = X."));
// 	Assert.AreEqual("X = [1]\r\n" + satisfied, globalInfo.ProcessInputString("?- X = '.'(1, '[]')."));
// 	Assert.AreEqual("X = [1]\r\n" + satisfied, globalInfo.ProcessInputString("?- '.'(1, '[]') = X."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- Vicky \== 'Vicky'."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- atom('Vicky').")); // Vicky is a variable; 'Vicky' is a functor (atom)
// }
//
// [Test]
// public void AtomTest() // 2014/04/04.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39
// {
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom(7)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom(7.0)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- atom(a)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom(a(b))."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom(X)."));
// }
//
// [Test]
// public void IntegerTest() // 2014/04/04
// {
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- integer(7)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- integer(7.0)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- integer(a)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- integer(a(b))."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- integer(X)."));
// }
//
// [Test]
// public void FloatTest() // 2014/04/04
// {
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- float(7)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- float(7.0)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- float(a)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- float(a(b))."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- float(X)."));
// }
//
// [Test]
// public void NumberTest() // 2014/04/04
// {
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- number(7)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- number(7.0)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- number(a)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- number(a(b))."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- number(X)."));
// }
//
// [Test]
// public void AtomicTest() // 2014/04/04
// {
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- atomic(7)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- atomic(7.0)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- atomic(a)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atomic(a(b))."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atomic(X)."));
// }
//
// [Test]
// public void VarTest()
// {
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- var(X)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- var(_)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- var(7)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- var(7.0)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- var(nil)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- var(a(b))."));
// }
//
// [Test]
// public void NonVarTest()
// {
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- nonvar(X)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- nonvar(_)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- nonvar(7)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- nonvar(7.0)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- nonvar(nil)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- nonvar(a(b))."));
// }
//
// [Test]
// public void GroundTest()
// {
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- ground(X)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- ground(_)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- ground(7)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- ground(7.0)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- ground(nil)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- ground(cons(1, cons(X, cons(3, nil))))."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- ground(cons(1, cons(2, cons(3, nil))))."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- ground([1, X, 3])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- ground([1, 2, 3])."));
// }
//
// [Test]
// public void PrefixNonArithmeticOperatorTest() // 2014/04/04
// {
// 	Assert.AreEqual("X = foo\r\n" + satisfied, globalInfo.ProcessInputString("?- =(X, foo)."));
// 	Assert.AreEqual("X = +(2, 3), Y = 5\r\n" + satisfied, globalInfo.ProcessInputString("?- =(2 + 3, X), is(Y, X)."));
// }
//
// [Test]
// public void PrefixArithmeticComparisonTest() // 2014/04/04
// {
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- <(0, 1)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- '<'(0, 1)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- <(0, 0)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- <(1, 0)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- <(1 + 2, 3 + 4)."));
// }
//
// [Test]
// public void Functor3Test() // 2014/04/05.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39
// {
// 	// This test must come first.
// 	Assert.AreEqual("T = f(Var1, Var2, Var3)\r\n" + satisfied, globalInfo.ProcessInputString(@"?- functor(T, f, 3)."));
//
// 	Assert.AreEqual("F = f, A = 2\r\n" + satisfied, globalInfo.ProcessInputString(@"?- functor(f(a, b), F, A)."));
// 	Assert.AreEqual("X = ., Y = 2\r\n" + satisfied, globalInfo.ProcessInputString(@"?- functor([a, b, c], X, Y)."));
// 	Assert.AreEqual("F = mia, A = 0\r\n" + satisfied, globalInfo.ProcessInputString(@"?- functor(mia, F, A)."));
// 	Assert.AreEqual("F = 8, A = 0\r\n" + satisfied, globalInfo.ProcessInputString(@"?- functor(8, F, A)."));
// 	Assert.AreEqual("F = 3.25, A = 0\r\n" + satisfied, globalInfo.ProcessInputString(@"?- functor(3.25, F, A)."));
// 	Assert.IsTrue(globalInfo.ProcessInputString(@"?- functor(T, f, 7).").EndsWith(satisfied));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString(@"?- functor(T, f, N)."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("complexterm(X) :- nonvar(X), functor(X, _, A), A  >  0."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- complexterm(f(a))."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- complexterm(f(a, b, c))."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- complexterm([1, 2, 3])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- complexterm(f)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- complexterm([])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- complexterm(7)."));
// 	//Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- complexterm(3.25)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- complexterm(X)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- complexterm(_)."));
// }
//
// [Test]
// public void Arg3Test() // 2014/04/05.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39
// {
// 	Assert.AreEqual("X = mia\r\n" + satisfied, globalInfo.ProcessInputString("?- arg(2, loves(vincent, mia), X)."));
// 	Assert.AreEqual("X = mia\r\n" + satisfied, globalInfo.ProcessInputString("?- arg(2, loves(vincent, X), mia)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- arg(2, happy(yolanda), X). "));
// }
//
// [Test]
// public void UnivTest() // 2014/04/05.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39
// {
// 	Assert.AreEqual("X = [loves, vincent, mia]\r\n" + satisfied, globalInfo.ProcessInputString("?- =..(loves(vincent, mia), X)."));
//
// 	// TODO 2014/04/05 : In order to remove the single quotes from around the univ operator, we would need a state-machine tokenizer : Done.
// 	Assert.AreEqual("X = [cause, vincent, dead(zed)]\r\n" + satisfied, globalInfo.ProcessInputString("?- cause(vincent, dead(zed)) =.. X."));
// 	Assert.AreEqual("X = a(b(c), d)\r\n" + satisfied, globalInfo.ProcessInputString("?- X =.. [a, b(c), d]."));
// 	Assert.AreEqual("X = [footmassage, Y, mia]\r\n" + satisfied, globalInfo.ProcessInputString("?- footmassage(Y, mia) =.. X."));
// 	Assert.AreEqual("X = [7]\r\n" + satisfied, globalInfo.ProcessInputString("?- 7 =.. X."));
// 	Assert.AreEqual("X = [7.5]\r\n" + satisfied, globalInfo.ProcessInputString("?- 7.5 =.. X."));
// }
//
// [Test]
// public void ArithmeticOperatorsAsFunctorsTest() // 2014/04/09.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse38
// {
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 2 + 3 == +(2, 3)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- +(2, 3) == 2 + 3."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 2 - 3 == -(2, 3)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- *(2, 3) == 2 * 3."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 2 * (7 + 2) == *(2, +(7, 2))."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- (2 < 3) == <(2, 3)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- (2 =< 3) == =<(2, 3)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- (2 =:= 3) == =:=(2, 3)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString(@"?- (2 =\= 3) == =\=(2, 3)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- (2 > 3) == >(2, 3)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- (2 >= 3) == >=(2, 3)."));
// }
//
// [Test]
// public void FSMTokenizerTest() // 2014/04/09.  Adapted from UnivTest.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39
// {
// 	// In order to remove the single quotes from around the univ operator, we use a state-machine tokenizer.
// 	Assert.AreEqual("X = [loves, vincent, mia]\r\n" + satisfied, globalInfo.ProcessInputString("?- =..(loves(vincent, mia), X)."));
//
// 	Assert.AreEqual("X = [cause, vincent, dead(zed)]\r\n" + satisfied,
// 		globalInfo.ProcessInputString("?- cause(vincent, dead(zed)) =.. X."));
// 	Assert.AreEqual("X = a(b(c), d)\r\n" + satisfied,
// 		globalInfo.ProcessInputString("?- X =.. [a, b(c), d]."));
// 	Assert.AreEqual("X = [footmassage, Y, mia]\r\n" + satisfied,
// 		globalInfo.ProcessInputString("?- footmassage(Y, mia) =.. X."));
// }
//
// [Test]
// public void AtomCodesTest() // 2014/04/09.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39
// {
// 	Assert.AreEqual("S = [86, 105, 99, 107, 121]\r\n" + satisfied, globalInfo.ProcessInputString("?- S = \"Vicky\"."));
// 	Assert.AreEqual("X = [118, 105, 99, 107, 121]\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_codes(vicky, X)."));
// 	Assert.AreEqual("X = [86, 105, 99, 107, 121]\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_codes('Vicky', X)."));
// 	Assert.AreEqual("X = [86, 105, 99, 107, 121, 32, 80, 111, 108, 108, 97, 114, 100]\r\n" + satisfied,
// 		globalInfo.ProcessInputString("?- atom_codes('Vicky Pollard', X)."));
// 	Assert.AreEqual("X = 107\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_codes(vicky, [118, 105, 99, X, 121])."));
// 	Assert.AreEqual("X = vicky\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_codes(X, [118, 105, 99, 107, 121])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- atom_codes(vicky, [118, 105, 99, 107, 121])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_codes(X, [118, 105, -1, 107, 121])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_codes(X, [])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_codes(7, [55])."));
//
// 	globalInfo.LoadPreset("append");
//
// 	Assert.AreEqual("X = [97, 98, 99], L = [97, 98, 99, 97, 98, 99], N = abcabc\r\n" + satisfied,
// 		globalInfo.ProcessInputString("?- atom_codes(abc, X), append(X, X, L), atom_codes(N, L)."));
// }
//
// [Test]
// public void NumberCodesTest() // 2014/04/09.  See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
// {
// 	Assert.AreEqual("L = [50, 51, 53, 55]\r\n" + satisfied, globalInfo.ProcessInputString("?- number_codes(2357, L)."));
// 	// atom_codes, number_codes, and name can all convert any atomic value to a list of codes.
// 	//Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- number_codes(foo, L)."));
// 	Assert.AreEqual("N = 2357\r\n" + satisfied, globalInfo.ProcessInputString("?- number_codes(N, [50, 51, 53, 55]), integer(N)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- number_codes(N, [50, 51, 65, 55])."));
// 	Assert.AreEqual("X = 53\r\n" + satisfied, globalInfo.ProcessInputString("?- number_codes(2357, [50, 51, X, 55])."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- number_codes(2357, [50, 51, 53, 55])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- number_codes(N, L)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- number_codes(N, [])."));
//
// 	// Floating-point number support:
// 	Assert.AreEqual("L = [49, 46, 48]\r\n" + satisfied, globalInfo.ProcessInputString("?- number_codes(1.0, L)."));
// 	Assert.AreEqual("N = 1.0\r\n" + satisfied, globalInfo.ProcessInputString("?- number_codes(N, [49, 46, 48]), float(N)."));
// 	Assert.AreEqual("X = 46\r\n" + satisfied, globalInfo.ProcessInputString("?- number_codes(1.0, [49, X, 48])."));
// }
//
// [Test]
// public void NameTest() // 2014/04/09.  See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
// {
// 	Assert.AreEqual("X = [118, 105, 99, 107, 121]\r\n" + satisfied, globalInfo.ProcessInputString("?- name(vicky, X)."));
// 	Assert.AreEqual("L = [50, 51, 53, 55]\r\n" + satisfied, globalInfo.ProcessInputString("?- name(2357, L)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- name(vicky(7), X)."));
// 	Assert.AreEqual("X = vicky\r\n" + satisfied, globalInfo.ProcessInputString("?- name(X, [118, 105, 99, 107, 121]), atom(X)."));
// 	Assert.AreEqual("N = 2357\r\n" + satisfied, globalInfo.ProcessInputString("?- name(N, [50, 51, 53, 55]), integer(N)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- name(X, [])."));
// }
//
// [Test]
// public void AtomCharsTest() // 2014/04/09.  See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
// {
// 	Assert.AreEqual("X = [v, i, c, k, y]\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_chars(vicky, X)."));
// 	Assert.AreEqual("X = vicky\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_chars(X, [v, i, c, k, y])."));
// 	Assert.AreEqual("X = k\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_chars(vicky, [v, i, c, X, y])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_chars(X, [])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_chars(7, ['7'])."));
// }
//
// [Test]
// public void NumberCharsTest() // 2014/04/09.  See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
// {
// 	// We assume that atom_chars and number_chars can both convert any atomic value to a list of chars.  See NumberCodesTest().
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- number_chars(2357, ['2', '3', '5', '7'])."));
// 	Assert.AreEqual("X = [2, 3, 5, 7]\r\n" + satisfied, globalInfo.ProcessInputString("?- number_chars(2357, X)."));
// 	Assert.AreEqual("X = 2357\r\n" + satisfied, globalInfo.ProcessInputString("?- number_chars(X, ['2', '3', '5', '7']), integer(X)."));
// 	Assert.AreEqual("X = 5\r\n" + satisfied, globalInfo.ProcessInputString("?- number_chars(2357, ['2', '3', X, '7']), atom(X)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- number_chars(N, L)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- number_chars(X, ['2', '3', a, '7'])."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- number_chars(X, [])."));
//
// 	// Floating-point number support:
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- number_chars(1.0, ['1', '.', '0'])."));
// 	Assert.AreEqual("L = [1, ., 0]\r\n" + satisfied, globalInfo.ProcessInputString("?- number_chars(1.0, L)."));
// 	Assert.AreEqual("N = 1.0\r\n" + satisfied, globalInfo.ProcessInputString("?- number_chars(N, ['1', '.', '0']), float(N)."));
// 	Assert.AreEqual("X = .\r\n" + satisfied, globalInfo.ProcessInputString("?- number_chars(1.0, ['1', X, '0'])."));
//
// }
//
// [Test]
// public void CharCodeTest() // 2014/04/10.  See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
// {
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- char_code(a, 97)."));
// 	Assert.AreEqual("N = 97\r\n" + satisfied, globalInfo.ProcessInputString("?- char_code(a, N)."));
// 	Assert.AreEqual("X = a\r\n" + satisfied, globalInfo.ProcessInputString("?- char_code(X, 97)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- char_code(ab, 97)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- char_code(a, 65)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- char_code(X, N)."));
// }
//
// [Test]
// public void CutInQueryTest() // 2014/04/10.  See the last part of exercise 1 on http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse46
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("p(1)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("p(2) :- !."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("p(3)."));
//
// 	globalInfo.FindAllSolutions();
//
// 	Assert.AreEqual("X = 1, Y = 1;\r\nX = 1, Y = 2;\r\n" + notSatisfied, globalInfo.ProcessInputString("?- p(X), !, p(Y)."));
// }
//
// [Test]
// public void GreenCutRedCutTest() // 2014/04/10.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse44
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("maxGreen(X, Y, Y) :- X =< Y, !."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("maxGreen(X, Y, X) :- X > Y."));
//
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("maxRed(X, Y, Z) :- X =< Y, !, Y = Z."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("maxRed(X, Y, X)."));
//
// 	Assert.AreEqual("X = 3\r\n" + satisfied, globalInfo.ProcessInputString("?- maxGreen(2, 3, X)."));
// 	Assert.AreEqual("X = 3\r\n" + satisfied, globalInfo.ProcessInputString("?- maxGreen(3, 2, X)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- maxGreen(2, 3, 3)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- maxGreen(3, 2, 3)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- maxGreen(2, 3, 2)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- maxGreen(3, 2, 2)."));
//
// 	Assert.AreEqual("X = 3\r\n" + satisfied, globalInfo.ProcessInputString("?- maxRed(2, 3, X)."));
// 	Assert.AreEqual("X = 3\r\n" + satisfied, globalInfo.ProcessInputString("?- maxRed(3, 2, X)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- maxRed(2, 3, 3)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- maxRed(3, 2, 3)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- maxRed(2, 3, 2)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- maxRed(3, 2, 2)."));
// }
//
// [Test]
// public void FloatingPointTest() // 2014/04/10
// {
// 	Assert.AreEqual("X = 5\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 2 + 3."));
// 	Assert.AreEqual("X = 5.25\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 2.25 + 3."));
// 	Assert.AreEqual("X = 5.5\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 2 + 3.5."));
// 	Assert.AreEqual("X = 5.75\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 2.25 + 3.5."));
// 	Assert.AreEqual("X = 6.0\r\n" + satisfied, globalInfo.ProcessInputString("?- X is 2.25 + 3.75."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 1 == 1.0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 1.0 == 1."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 1 =:= 1.0."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 1.0 =:= 1."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 1 < 1."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 1 < 1.0."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 1.0 < 1."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 1.0 < 1.0."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 2 < 3."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 3 < 2."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 2.25 < 3."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 2.25 < 2."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 2 < 2.25."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 2 < 1.25."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- 2.25 < 3.5."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- 3.5 < 2.25."));
// }
//
// [Test]
// public void AtomNumberTest() // 2014/04/11.  See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
// {
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_number(7, 7)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- atom_number('7', 7)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_number('7', 6)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- atom_number('7.0', 7.0)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- atom_number('7.5', 7.5)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_number('7.0', 7)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_number('7', 7.0)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_number('7.25', 7.0)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_number('7.25', 7)."));
// 	Assert.AreEqual("X = 7\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_number('7', X), integer(X)."));
// 	Assert.AreEqual("X = 7.0\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_number('7.0', X), float(X)."));
// 	Assert.AreEqual("X = 7.5\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_number('7.5', X), float(X)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_number(a, X)."));
// 	Assert.AreEqual("X = 7\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_number(X, 7), atom(X)."));
// 	Assert.AreEqual("X = 7.0\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_number(X, 7.0), atom(X)."));
// 	Assert.AreEqual("X = 7.5\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_number(X, 7.5), atom(X)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_number(X, Y)."));
// }
//
// [Test]
// public void AtomConcatTest() // 2014/04/12.  See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
// {
// 	globalInfo.LoadPreset("atom_concat");
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- atom_concat(ab, cde, abcde)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_concat(ab, cde, abfcde)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- atom_concat(ab, 7, ab7)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- atom_concat(ab, 7.0, 'ab7.0')."));
// 	Assert.AreEqual("A3 = abcde\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_concat(ab, cde, A3)."));
// 	Assert.AreEqual("A3 = ab7\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_concat(ab, 7, A3)."));
// 	Assert.AreEqual("A3 = 7ab\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_concat(7, ab, A3)."));
// 	Assert.AreEqual("A2 = cde\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_concat(ab, A2, abcde)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_concat(abf, A2, abcde)."));
// 	Assert.AreEqual("A1 = ab\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_concat(A1, cde, abcde)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_concat(A1, fcde, abcde)."));
// 	Assert.AreEqual("A1 = ab\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_concat(A1, '7', ab7)."));
//
// 	globalInfo.FindAllSolutions();
// 	Assert.AreEqual(@"A1 = a, A2 = bcde;
// A1 = ab, A2 = cde;
// A1 = abc, A2 = de;
// A1 = abcd, A2 = e;
// " + notSatisfied, globalInfo.ProcessInputString("?- atom_concat(A1, A2, abcde)."));
// }
//
// [Test]
// public void AtomLengthTest() // 2014/04/12.  See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
// {
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- atom_length(abc, 3)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- atom_length(\"abc\", 3)."));   // atom_length works for strings too.
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- atom_length(\"\", 0)."));      // The empty string, not the functor '[]'.
// 	Assert.AreEqual("L = 3\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_length(abc, L)."));
// 	Assert.AreEqual("L = 3\r\n" + satisfied, globalInfo.ProcessInputString("?- atom_length(\"abc\", L)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- atom_length(A, L)."));
// }
//
// [Test]
// public void ConcatAtomTest() // 2014/04/12.  See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
// {
// 	globalInfo.LoadPreset("concat_atom");
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- concat_atom([], X)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- concat_atom([abc], abc)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- concat_atom([ab, cde], abcde)."));
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- concat_atom([ab, cde, fg], abcdefg)."));
// 	Assert.AreEqual("X = abc\r\n" + satisfied, globalInfo.ProcessInputString("?- concat_atom([abc], X)."));
// 	Assert.AreEqual("X = abcde\r\n" + satisfied, globalInfo.ProcessInputString("?- concat_atom([ab, cde], X)."));
// 	Assert.AreEqual("X = abcdefg\r\n" + satisfied, globalInfo.ProcessInputString("?- concat_atom([ab, cde, fg], X)."));
// 	Assert.AreEqual("X = ab\r\n" + satisfied, globalInfo.ProcessInputString("?- concat_atom([X, cde], abcde)."));
// 	Assert.AreEqual("X = cde\r\n" + satisfied, globalInfo.ProcessInputString("?- concat_atom([ab, X], abcde)."));
//
// 	globalInfo.FindAllSolutions();
// 	Assert.AreEqual(@"A1 = a, A2 = bcde;
// A1 = ab, A2 = cde;
// A1 = abc, A2 = de;
// A1 = abcd, A2 = e;
// " + notSatisfied, globalInfo.ProcessInputString("?- concat_atom([A1, A2], abcde)."));
// }
//
// [Test]
// public void FindAllTest() // 2014/04/14.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse49
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("foo(bar)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("foo(baz)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("foo(bat)."));
//
// 	Assert.AreEqual("Result = [bar, baz, bat]\r\n" + satisfied, globalInfo.ProcessInputString("?- findall(X, foo(X), Result)."));
//
// 	// This query is satisfied, and returns the empty list (unlike bagof/3) :
// 	Assert.AreEqual("Result = []\r\n" + satisfied, globalInfo.ProcessInputString("?- findall(X, blip(X), Result)."));
// }
//
// [Test]
// public void BagOfTest() // 2014/04/15.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse49
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("child(martha, charlotte)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("child(charlotte, caroline)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("child(caroline, laura)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("child(laura, rose)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("descend(X, Y) :- child(X, Y)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("descend(X, Y) :- child(X, Z), descend(Z, Y)."));
//
// 	globalInfo.FindAllSolutions();
// 	Assert.AreEqual(@"Mother = caroline, List = [laura, rose];
// Mother = charlotte, List = [caroline, laura, rose];
// Mother = laura, List = [rose];
// Mother = martha, List = [charlotte, caroline, laura, rose];
// " + notSatisfied, globalInfo.ProcessInputString("?- bagof(Child, descend(Mother, Child), List)."));
//
// 	globalInfo.FindFirstSolution();
// 	Assert.AreEqual("List = [charlotte, caroline, laura, rose, caroline, laura, rose, laura, rose, rose]\r\n" + satisfied,
// 		globalInfo.ProcessInputString("?- bagof(Child, Mother ^ descend(Mother, Child), List)."));
// 	Assert.AreEqual(notSatisfied, globalInfo.ProcessInputString("?- bagof(X, descend(mary, X), Z).")); // Instead of being satisfied and returning the empty list.
// 	Assert.AreEqual("Z = [[laura, rose], [caroline, laura, rose], [rose], [charlotte, caroline, laura, rose]]\r\n" + satisfied,
// 		globalInfo.ProcessInputString("?- findall(List, bagof(Child, descend(Mother, Child), List), Z)."));
// 	Assert.AreEqual("Z = [[laura, rose], [caroline, laura, rose], [rose], [charlotte, caroline, laura, rose]]\r\n" + satisfied,
// 		globalInfo.ProcessInputString("?- bagof(List, Child ^ Mother ^ bagof(Child, descend(Mother, Child), List), Z)."));
// }
//
// [Test]
// public void SetOfTest() // 2014/04/15.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse49
// {
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("age(harry, 13)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("age(draco, 14)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("age(ron, 13)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("age(hermione, 13)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("age(dumbledore, 60)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("age(hagrid, 30)."));
//
// 	Assert.AreEqual("Out = [draco, dumbledore, hagrid, harry, hermione, ron]\r\n" + satisfied,
// 		globalInfo.ProcessInputString("?- setof(X, Y ^ age(X, Y), Out)."));
// 	Assert.AreEqual("Out = [13, 14, 30, 60]\r\n" + satisfied, globalInfo.ProcessInputString("?- setof(Y, X ^ age(X, Y), Out)."));
// }
//
// [Test]
// public void UserDefinedOperatorTest() // 2014/05/06.  See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse40
// {
// 	Assert.AreEqual(operatorAdded, globalInfo.ProcessInputString(":- op(500, xf, is_dead)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("kill(marsellus, zed)."));
// 	Assert.AreEqual(clauseAdded, globalInfo.ProcessInputString("is_dead(X) :- kill(_, X)."));
//
// 	Assert.AreEqual(satisfied, globalInfo.ProcessInputString("?- zed is_dead."));
// }
