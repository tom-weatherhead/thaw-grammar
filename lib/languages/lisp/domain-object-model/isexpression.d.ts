export interface ISExpression {
    isNumber(): boolean;
    isSymbol(): boolean;
    isList(): boolean;
    isNull(): boolean;
    isPrimOp(): boolean;
    isClosure(): boolean;
    isString(): boolean;
    toString(): string;
}
