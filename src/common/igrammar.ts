// tom-weatherhead/thaw-grammar/src/common/igrammar.ts

// import { Stack } from 'thaw-common-utilities.ts';
//
// import { Token } from 'thaw-lexical-analyzer';
//
// import { Production } from './production';
//
// import { Symbol } from './symbol';
//
// /* eslint-disable @typescript-eslint/ban-types */
// export interface IGrammar {
// 	terminals: Symbol[];
// 	nonTerminals: Symbol[];
// 	startSymbol: Symbol;
// 	productions: Production[];
//
// 	languageName: string; // This is a 'get' accessor.
// 	selectorsOfCompatibleParsers: number[]; // An array of members of the enum ParserSelector
// 	// eslint-disable-next-line @typescript-eslint/no-explicit-any
// 	executeSemanticAction(semanticStack: Stack<any>, action: string): void;
// 	tokenToSymbol(token: Token): Symbol;
// 	pushTokenOntoSemanticStack(
// 		// eslint-disable-next-line @typescript-eslint/no-explicit-any
// 		semanticStack: Stack<any>,
// 		tokenAsSymbol: Symbol,
// 		token: Token
// 	): void;
// 	findStartingProduction(): Production;
// 	// removeProductionsContainingSymbol(symbol: number): void;
// }
// /* eslint-enable @typescript-eslint/ban-types */
