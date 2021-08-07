// tom-weatherhead/thaw-grammar/src/languages/json/json-grammar.ts

import { Stack } from 'thaw-common-utilities.ts';

import { Token } from 'thaw-lexical-analyzer';

import { GrammarBase } from '../../common/grammar-base';
import { ParserSelector } from '../../common/parser-selectors';
import { Symbol } from '../../common/symbol';

export class JSONGrammar extends GrammarBase {
	// A grammar for JSON (JavaScript Object Notation)

	constructor() {
		super(Symbol.nonterminalStart);

		// ...
	}

	public get languageName(): string {
		return 'JSON';
	}

	public get selectorsOfCompatibleParsers(): number[] {
		return [ParserSelector.LL1];
	}

	// eslint-disable-next-line @typescript-eslint/no-explicit-any, @typescript-eslint/no-unused-vars
	public executeSemanticAction(semanticStack: Stack<any>, action: string): void {}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public tokenToSymbol(token: Token): number {
		// Returns Symbol
		return Symbol.UndefinedSymbol;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public pushTokenOntoSemanticStack(
		// eslint-disable-next-line @typescript-eslint/no-explicit-any
		semanticStack: Stack<any>,
		tokenAsSymbol: number,
		token: Token
	): void {}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
