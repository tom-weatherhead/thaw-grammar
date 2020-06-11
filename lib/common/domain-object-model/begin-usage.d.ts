import { EnvironmentFrame } from './environment-frame';
import { ExpressionList } from './expression-list';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
export declare class BeginUsage<T> implements IExpression<T> {
    readonly firstExpression: IExpression<T>;
    readonly expressionList: ExpressionList<T>;
    constructor(firstExpression: IExpression<T>, expressionList: ExpressionList<T>);
    toString(): string;
    evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T;
}
