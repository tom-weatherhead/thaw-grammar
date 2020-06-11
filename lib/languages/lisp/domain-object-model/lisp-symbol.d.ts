import { SExpressionBase } from './sexpression-base';
export declare class LISPSymbol extends SExpressionBase {
    readonly value: string;
    constructor(value: string);
    toString(): string;
    isSymbol(): boolean;
}
