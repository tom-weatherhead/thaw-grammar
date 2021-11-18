// tom-weatherhead/thaw-parser/test/lisp.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { createFnRecognizer, createInfrastructure } from '../../create-infrastructure';

import { createGrammar, IExpression, ISExpression, LISPGlobalInfo } from '../../..';

import { createParser, SyntaxException } from 'thaw-parser';

const ls = LanguageSelector.LISP;

test('LL(1) LISP parser instance creation test', () => {
	// Arrange
	const grammar = createGrammar(ls);

	// Act
	const parser = createParser(grammar.defaultParser, grammar);

	// Assert
	expect(parser).toBeTruthy();
});

test('LL(1) LISP recognize test', () => {
	// 	// Arrange
	// const ls = LanguageSelector.LISP;
	// const grammar = createGrammar(ls);
	// const tokenizer = createTokenizer(grammar.defaultLexicalAnalyzer, ls);
	// const parser = createParser(grammar.defaultParser, grammar);
	//
	// const f = (str: string): void => parser.recognize(tokenizer.tokenize(str));
	const f = createFnRecognizer(ls);

	// f('pred1.');

	// expect(() => f('pred1(A.')).toThrow(ParserException);

	f('(* 7 13)');

	expect(() => f('(* 7 13')).toThrow(SyntaxException);
});

function lispTest(data: Array<[input: string, expectedResult: string | string[]]>): void {
	// Arrange
	// const grammar = createGrammar(ls);
	// const tokenizer = createTokenizer(LexicalAnalyzerSelector.MidnightHack, ls);
	// const parser = createParser(ParserSelector.LL1, grammar);
	const { tokenizer, parser } = createInfrastructure(ls);
	const lispGlobalInfo = new LISPGlobalInfo();

	for (const [input, expectedResult] of data) {
		// Act
		const parseResult = parser.parse(tokenizer.tokenize(input));
		const expr = parseResult as IExpression<ISExpression>;
		const actualResult = expr
			.evaluate(lispGlobalInfo.globalEnvironment, lispGlobalInfo)
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

test('LL(1) LISP addition test 1', () => {
	lispTest([['(+ 2 3)', '5']]);
});
