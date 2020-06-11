import { Stack } from 'thaw-common-utilities.ts';
import { Token } from 'thaw-lexical-analyzer';
import { IGrammar } from './igrammar';
import { Production } from './production';
export declare abstract class GrammarBase implements IGrammar {
    readonly terminals: number[];
    readonly nonTerminals: number[];
    readonly startSymbol: number;
    readonly productions: Production[];
    protected constructor(startSymbol: number);
    abstract get languageName(): string;
    abstract get selectorsOfCompatibleParsers(): number[];
    abstract executeSemanticAction(semanticStack: Stack<any>, action: string): void;
    abstract tokenToSymbol(token: Token): number;
    abstract pushTokenOntoSemanticStack(semanticStack: Stack<any>, tokenAsSymbol: number, token: Token): void;
    findStartingProduction(): Production;
    protected addProduction(lhs: number, rhs: any[], n?: number): void;
}
