// tom-weatherhead/thaw-grammar/src/languages/json/json-grammar.ts

// import { Stack } from 'thaw-common-utilities.ts';

import {
	GrammarSymbol,
	IToken,
	// LexicalState,
	ParserSelector,
	SemanticStackType
} from 'thaw-interpreter-types';

// import { Token } from 'thaw-lexical-analyzer';

import { GrammarBase } from 'thaw-interpreter-core';
// import { ParserSelector } from '../../common/parser-selectors';
// import { Symbol } from '../../common/symbol';

export class JSONGrammar extends GrammarBase {
	// A grammar for JSON (JavaScript Object Notation)

	constructor() {
		super(GrammarSymbol.nonterminalStart);

		// ...
	}

	public get languageName(): string {
		return 'JSON';
	}

	public get selectorsOfCompatibleParsers(): ParserSelector[] {
		return [ParserSelector.LL1];
	}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public tokenToSymbol(token: IToken): GrammarSymbol {
		return GrammarSymbol.UndefinedSymbol;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public pushTokenOntoSemanticStack(
		semanticStack: SemanticStackType,
		tokenAsSymbol: GrammarSymbol,
		token: IToken
	): void {}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
