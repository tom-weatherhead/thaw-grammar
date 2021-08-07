// tom-weatherhead/thaw-grammar/src/languages/clu/clu-grammar.ts

import { Stack } from 'thaw-common-utilities.ts';

import { Token } from 'thaw-lexical-analyzer';

import { GrammarBase } from '../../common/grammar-base';
import { ParserSelector } from '../../common/parser-selectors';
import { Symbol } from '../../common/symbol';

export class CluGrammar extends GrammarBase {
	// The CLU grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')

	constructor() {
		super(Symbol.nonterminalStart);

		// ...
	}

	public get languageName(): string {
		return 'Clu';
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
