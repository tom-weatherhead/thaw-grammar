// tom-weatherhead/thaw-grammar/src/common/igrammar.ts

import { Stack } from 'thaw-common-utilities.ts';

import { Token } from 'thaw-lexical-analyzer';

import { Production } from './production';

import { Symbol } from './symbol';

/* eslint-disable @typescript-eslint/ban-types */
export interface IGrammar {
	terminals: Symbol[]; // Symbol[]
	nonTerminals: Symbol[]; // Symbol[]
	startSymbol: Symbol; // Symbol
	productions: Production[];

	languageName: string; // This is a 'get' accessor.
	selectorsOfCompatibleParsers: number[]; // An array of members of the enum ParserSelector
	executeSemanticAction(semanticStack: Stack<any>, action: string): void;
	tokenToSymbol(token: Token): Symbol;
	pushTokenOntoSemanticStack(
		semanticStack: Stack<any>,
		tokenAsSymbol: Symbol,
		token: Token
	): void;
	findStartingProduction(): Production;
	// removeProductionsContainingSymbol(symbol: number): void;
}
/* eslint-enable @typescript-eslint/ban-types */
