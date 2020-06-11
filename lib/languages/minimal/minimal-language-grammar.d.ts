import { Stack } from 'thaw-common-utilities.ts';
import { Token } from 'thaw-lexical-analyzer';
import { GrammarBase } from '../../common/grammar-base';
export declare class MinimalLanguageGrammar extends GrammarBase {
    constructor();
    get languageName(): string;
    get selectorsOfCompatibleParsers(): number[];
    executeSemanticAction(semanticStack: Stack<any>, action: string): void;
    tokenToSymbol(token: Token): number;
    pushTokenOntoSemanticStack(semanticStack: Stack<any>, tokenAsSymbol: number, token: Token): void;
}
