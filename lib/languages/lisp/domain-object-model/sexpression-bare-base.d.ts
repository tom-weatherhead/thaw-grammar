import { ISExpression } from './isexpression';
export declare abstract class SExpressionBareBase implements ISExpression {
    isNumber(): boolean;
    isSymbol(): boolean;
    isList(): boolean;
    isNull(): boolean;
    isPrimOp(): boolean;
    isClosure(): boolean;
    isString(): boolean;
    abstract toString(): string;
}
