import { EnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
export declare class CondUsage<T> implements IExpression<T> {
    readonly exprPairList: Array<[IExpression<T>, IExpression<T>]>;
    constructor(exprPairList: Array<[IExpression<T>, IExpression<T>]>);
    toString(): string;
    evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T;
}
