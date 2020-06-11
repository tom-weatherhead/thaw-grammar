import { SExpressionBase } from './sexpression-base';
export declare class LISPString extends SExpressionBase {
    readonly value: string;
    constructor(value: string);
    toString(): string;
    isString(): boolean;
}
