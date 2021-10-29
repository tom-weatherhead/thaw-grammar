// tom-weatherhead/thaw-grammar/test/create-infrastructure.ts

'use strict';

import { IGrammar, IParser, ITokenizer, LanguageSelector } from 'thaw-interpreter-types';

import { createTokenizer } from 'thaw-lexical-analyzer';

import { createParser } from 'thaw-parser';

import { createGrammar } from '..';

export interface IInterpreterInfrastructure {
	readonly grammar: IGrammar;
	readonly tokenizer: ITokenizer;
	readonly parser: IParser;
}

export function createInfrastructure(ls: LanguageSelector): IInterpreterInfrastructure {
	const grammar = createGrammar(ls);
	const tokenizer = createTokenizer(grammar.defaultLexicalAnalyzer, ls);
	const parser = createParser(grammar.defaultParser, grammar);

	return { grammar, tokenizer, parser };
}
