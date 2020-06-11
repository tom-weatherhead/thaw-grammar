import { IExpression } from './iexpression';
export declare class ExpressionList<T> {
    readonly value: Array<IExpression<T>>;
    toString(): string;
}
