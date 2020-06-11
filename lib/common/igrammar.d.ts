import { Stack } from 'thaw-common-utilities.ts';
import { Token } from 'thaw-lexical-analyzer';
import { Production } from './production';
export interface IGrammar {
    terminals: number[];
    nonTerminals: number[];
    startSymbol: number;
    productions: Production[];
    languageName: string;
    selectorsOfCompatibleParsers: number[];
    executeSemanticAction(semanticStack: Stack<any>, action: string): void;
    tokenToSymbol(token: Token): number;
    pushTokenOntoSemanticStack(semanticStack: Stack<any>, tokenAsSymbol: number, token: Token): void;
    findStartingProduction(): Production;
}
