// tom-weatherhead/thaw-parser/test/sasl.test.ts

'use strict';

import { LanguageSelector, LexicalAnalyzerSelector, ParserSelector } from 'thaw-interpreter-types';

import { createTokenizer } from 'thaw-lexical-analyzer';

import { createGrammar, IExpression, ISExpression, SASLGlobalInfo } from '../../..';

import { createParser, SyntaxException } from 'thaw-parser';

test('LL(1) SASL parser instance creation test', () => {
	// Arrange
	const ls = LanguageSelector.SASL;
	const grammar = createGrammar(ls);

	// Act
	const parser = createParser(ParserSelector.LL1, grammar);

	// Assert
	expect(parser).toBeTruthy();
});

test('LL(1) SASL recognize test', () => {
	// Arrange
	const ls = LanguageSelector.SASL;
	const grammar = createGrammar(ls);
	const tokenizer = createTokenizer(LexicalAnalyzerSelector.MidnightHack, ls);
	const parser = createParser(ParserSelector.LL1, grammar);

	const f = (str: string): void => parser.recognize(tokenizer.tokenize(str));

	f('(* 7 13)');

	expect(() => f('(* 7 13')).toThrow(SyntaxException);
});

function saslTest(data: Array<[input: string, expectedResult: string | string[]]>): void {
	// Arrange
	const ls = LanguageSelector.SASL;
	const grammar = createGrammar(ls);
	const tokenizer = createTokenizer(LexicalAnalyzerSelector.MidnightHack, ls);
	const parser = createParser(ParserSelector.LL1, grammar);
	const saslGlobalInfo = new SASLGlobalInfo({ tokenizer, parser });

	for (const [input, expectedResult] of data) {
		// Act
		const parseResult = parser.parse(tokenizer.tokenize(input));
		const expr = parseResult as IExpression<ISExpression>;
		const actualResult = expr
			.evaluate(saslGlobalInfo, saslGlobalInfo.globalEnvironment)
			.toString();

		// console.log(`input: ${input}\nactualResult:\n${actualResult}\n\n`);

		// Assert
		if (typeof expectedResult === 'string') {
			expect(actualResult).toBe(expectedResult);
		} else {
			for (const str of expectedResult) {
				expect(actualResult.includes(str)).toBe(true);
			}
		}
	}
}

test('LL(1) SASL addition test 1', () => {
	saslTest([['(+ 2 3)', '5']]);
});

test('SASL infinite list test', () => {
	saslTest([
		['(set +1 (lambda (n) (+ n 1)))', '<closure>'],
		['(set ints-from (lambda (i) (cons i (ints-from (+1 i)))))', '<closure>'],
		['(set ints (ints-from 0))', '(<thunk> <thunk>)'],
		['ints', '(<thunk> <thunk>)'],
		['(car ints)', '0'],
		['ints', '(0 <thunk>)'],
		['(car (cdr ints))', '1'],
		['ints', '(0 (1 <thunk>))']
	]);
});
