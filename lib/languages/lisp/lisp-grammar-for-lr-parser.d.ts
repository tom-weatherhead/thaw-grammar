import { Stack } from 'thaw-common-utilities.ts';
import { Token } from 'thaw-lexical-analyzer';
import { IExpression } from '../../common/domain-object-model/iexpression';
import { Name } from '../../common/domain-object-model/name';
import { Variable } from '../../common/domain-object-model/variable';
import { GrammarBase } from '../../common/grammar-base';
import { ISExpression } from './domain-object-model/isexpression';
export declare class LISPGrammarForLRParser extends GrammarBase {
    constructor();
    get languageName(): string;
    get selectorsOfCompatibleParsers(): number[];
    executeSemanticAction(semanticStack: Stack<any>, action: string): void;
    tokenToSymbol(token: Token): number;
    pushTokenOntoSemanticStack(semanticStack: Stack<any>, tokenAsSymbol: number, token: Token): void;
    protected createLetUsage(letName: Name, varExprList: Array<[Variable<ISExpression>, IExpression<ISExpression>]>, expression: IExpression<ISExpression>): IExpression<ISExpression>;
}
