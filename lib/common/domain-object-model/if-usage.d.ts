import { EnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
export declare class IfUsage<T> implements IExpression<T> {
    readonly condition: IExpression<T>;
    readonly ifBody: IExpression<T>;
    readonly elseBody: IExpression<T>;
    constructor(condition: IExpression<T>, ifBody: IExpression<T>, elseBody: IExpression<T>);
    toString(): string;
    evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T;
}
