// tom-weatherhead/thaw-grammar/src/languages/clu/clu-grammar.ts

// import { Stack } from 'thaw-common-utilities.ts';

import {
	GrammarSymbol,
	IToken,
	// LanguageSelector,
	// LexicalState,
	ParserSelector,
	SemanticStackType
} from 'thaw-interpreter-types';

// import { Token } from 'thaw-lexical-analyzer';

import { GrammarBase } from 'thaw-interpreter-core';
// import { ParserSelector } from '../../common/parser-selectors';
// import { Symbol } from '../../common/symbol';

export class CluGrammar extends GrammarBase {
	// The CLU grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')

	constructor() {
		super(GrammarSymbol.nonterminalStart);

		// ...
	}

	public get languageName(): string {
		return 'Clu';
	}

	public get selectorsOfCompatibleParsers(): ParserSelector[] {
		return [ParserSelector.LL1];
	}

	// eslint-disable-next-line @typescript-eslint/no-explicit-any, @typescript-eslint/no-unused-vars
	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public tokenToSymbol(token: IToken): GrammarSymbol {
		// Returns Symbol
		return GrammarSymbol.UndefinedSymbol;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public pushTokenOntoSemanticStack(
		// eslint-disable-next-line @typescript-eslint/no-explicit-any
		semanticStack: SemanticStackType,
		tokenAsSymbol: number,
		token: IToken
	): void {}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
