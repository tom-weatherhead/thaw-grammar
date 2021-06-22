// tom-weatherhead/thaw-grammar/src/common/igrammar.ts

import { Stack } from 'thaw-common-utilities.ts';

import { Token } from 'thaw-lexical-analyzer';

import { Production } from './production';

export interface IGrammar {
	terminals: number[]; // Symbol[]
	nonTerminals: number[]; // Symbol[]
	startSymbol: number; // Symbol
	productions: Production[];

	languageName: string; // This is a 'get' accessor.
	selectorsOfCompatibleParsers: number[]; // An array of members of the enum ParserSelector
	executeSemanticAction(semanticStack: Stack<any>, action: string): void;
	tokenToSymbol(token: Token): number;
	pushTokenOntoSemanticStack(
		semanticStack: Stack<any>,
		tokenAsSymbol: number,
		token: Token
	): void;
	findStartingProduction(): Production;
	// removeProductionsContainingSymbol(symbol: number): void;
}
