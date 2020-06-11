import { ISExpression } from './isexpression';
import { SExpressionBase } from './sexpression-base';
export declare class SExpressionList extends SExpressionBase {
    static makeFromList(l: ISExpression[]): ISExpression;
    private static makeFromListHelper;
    head: ISExpression;
    tail: ISExpression;
    constructor(head: ISExpression, tail: ISExpression);
    toString(): string;
    isList(): boolean;
    getLength(): number;
    private toStringWithoutBrackets;
}
